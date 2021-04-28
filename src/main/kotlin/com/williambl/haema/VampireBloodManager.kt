package com.williambl.haema

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes
import com.williambl.haema.abilities.VampireAbility
import com.williambl.haema.api.BloodChangeEvents
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.damagesource.BloodLossDamageSource
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.effect.VampiricWeaknessEffect
import com.williambl.haema.util.computeValueWithout
import com.williambl.haema.util.feedCooldown
import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.fabricmc.fabric.api.tag.TagRegistry
import net.minecraft.entity.EntityType
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.player.HungerManager
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundTag
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.sound.SoundEvents
import net.minecraft.tag.Tag
import net.minecraft.util.ActionResult
import net.minecraft.util.Identifier
import net.minecraft.world.GameRules
import net.minecraft.world.World
import java.util.*
import kotlin.math.*

@Suppress("DEPRECATION")
class VampireBloodManager() : HungerManager() {

    constructor(original: HungerManager) : this() {
        absoluteBloodLevel = original.foodLevel.toDouble()
    }

    companion object {
        private val VAMPIRE_REACH_UUID = UUID.fromString("0eb4fc5f-71d5-4440-b517-bcc18e1df6f4")
        private val VAMPIRE_ATTACK_RANGE_UUID = UUID.fromString("3267a46b-2b48-429f-a3a8-439aa87a876d")
        private val VAMPIRE_HEALTH_BOOST_UUID = UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92")
        private val VAMPIRE_REACH = EntityAttributeModifier(VAMPIRE_REACH_UUID, "Vampire reach extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
        private val VAMPIRE_ATTACK_RANGE = EntityAttributeModifier(VAMPIRE_ATTACK_RANGE_UUID, "Vampire attack range extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
        private val VAMPIRE_HEALTH_BOOST = EntityAttributeModifier(VAMPIRE_HEALTH_BOOST_UUID, "Vampire health boost", 1.0, EntityAttributeModifier.Operation.MULTIPLY_BASE)

        val goodBloodTag: Tag<EntityType<*>> = TagRegistry.entityType(Identifier("haema:good_blood_sources"))
        val mediumBloodTag: Tag<EntityType<*>> = TagRegistry.entityType(Identifier("haema:medium_blood_sources"))
        val poorBloodTag: Tag<EntityType<*>> = TagRegistry.entityType(Identifier("haema:poor_blood_sources"))

        val bloodLevelPacket = Identifier("haema:bloodlevelsync")

        fun getFeedCooldown(world: World): Int = world.gameRules[feedCooldown].get()
    }

    var owner: PlayerEntity? = null

    @Deprecated("use getBloodLevel()")
    var absoluteBloodLevel: Double = 7.0

    var lastFed: Long = -24000
    var invisTicks: Long = 0

    override fun update(player: PlayerEntity?) {
        owner = player!!

        player.isSilent = (getBloodLevel() >= 10 && player.isSprinting) || getBloodLevel() >= 12

        if (getBloodLevel() > 3 && player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)?.hasModifier(VAMPIRE_HEALTH_BOOST) == false) {
            player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)!!.addPersistentModifier(VAMPIRE_HEALTH_BOOST)
        }

        if (getBloodLevel() <= 3) {
            player.addStatusEffect(StatusEffectInstance(VampiricWeaknessEffect.instance, 5, 3 - getBloodLevel().roundToInt(), false, false, true))
            if (player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)?.hasModifier(VAMPIRE_HEALTH_BOOST) == true) {
                player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)!!.removeModifier(VAMPIRE_HEALTH_BOOST)
            }
        }

        val reachAttr = player.getAttributeInstance(ReachEntityAttributes.REACH)
        val attackRangeAttr = player.getAttributeInstance(ReachEntityAttributes.ATTACK_RANGE)

        if (getBloodLevel() >= 6 && (reachAttr?.hasModifier(VAMPIRE_REACH) == false || attackRangeAttr?.hasModifier(
                        VAMPIRE_ATTACK_RANGE
                ) == false)
        ) {
            reachAttr?.addTemporaryModifier(VAMPIRE_REACH)
            attackRangeAttr?.addTemporaryModifier(VAMPIRE_ATTACK_RANGE)
        } else if (reachAttr?.hasModifier(VAMPIRE_REACH) != false || attackRangeAttr?.hasModifier(VAMPIRE_ATTACK_RANGE) != false) {
            reachAttr?.removeModifier(VAMPIRE_REACH)
            attackRangeAttr?.removeModifier(VAMPIRE_ATTACK_RANGE)
        }


        if (getBloodLevel() >= 10 && (player as Vampirable).getAbilityLevel(VampireAbility.STRENGTH) > 0) {
            player.addStatusEffect(StatusEffectInstance(VampiricStrengthEffect.instance, 40, when {
                getBloodLevel() >= 19 -> 2
                getBloodLevel() >= 14 -> 1
                else -> 0
            }.coerceAtMost((player as Vampirable).getAbilityLevel(VampireAbility.STRENGTH)-1), false, false, true))
        }

        val invisLevel = (player as Vampirable).getAbilityLevel(VampireAbility.INVISIBILITY)
        if (getBloodLevel() >= 16 && invisLevel > 0 && player.isSneaking && player.world.time-invisTicks >= 120 + invisLevel*60) {
            invisTicks = player.world.time
            player.addStatusEffect(StatusEffectInstance(StatusEffects.INVISIBILITY, invisLevel*60, 0))
            ServerPlayNetworking.send(player as ServerPlayerEntity, Identifier("haema:updateinvisticks"), PacketByteBuf(Unpooled.buffer()))
        }

        //Healing at the bottom, so that the health boosts aren't wiped
        if (getBloodLevel() >= 8 || (getBloodLevel() > 0 && player.health <= 0 && player.isAlive)) {
            if (player.world.gameRules.get(GameRules.NATURAL_REGENERATION).get() && player.canFoodHeal()) {
                val defaultMaxHealth = player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)?.computeValueWithout(UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92")) ?: 20.0
                if (player.health >= defaultMaxHealth) {
                    if (player.age % 20 == 0 && (player.health - defaultMaxHealth) < when {
                            getBloodLevel() >= 19 -> 20
                            getBloodLevel() >= 14 -> 10
                            getBloodLevel() >= 10 -> 6
                            else -> 0
                        }) {
                        heal(player)
                    }
                } else {
                    heal(player)
                }
            } else if (player.health <= 0) {
                player.health = 1f
                removeBlood(1.0)
            }
        }
        sync(player)
    }

    @Deprecated("use blood")
    override fun add(food: Int, f: Float) {}
    @Deprecated("use blood")
    override fun addExhaustion(exhaustion: Float) {}
    @Deprecated("use blood")
    override fun eat(item: Item?, stack: ItemStack?) {}
    @Deprecated("use blood")
    override fun setFoodLevel(foodLevel: Int) {}
    @Deprecated("use blood", ReplaceWith("getIntBloodLevel()"))
    override fun getFoodLevel(): Int {
        return getIntBloodLevel()
    }

    override fun fromTag(tag: CompoundTag?) {
        super.fromTag(tag)
        absoluteBloodLevel = tag?.getDouble("BloodLevel") ?: 0.0
        lastFed = tag?.getLong("LastFed") ?: -6000
    }

    override fun toTag(tag: CompoundTag?) {
        super.toTag(tag)
        tag?.putDouble("BloodLevel", absoluteBloodLevel)
        tag?.putLong("LastFed", lastFed)
    }

    fun getIntBloodLevel(): Int {
        return floor(getBloodLevel()).toInt()
    }

    fun getBloodLevel(): Double {
        return if (owner?.isCreative == true) 20.0 else 20.0 * (sin((absoluteBloodLevel * PI) / 40.0))
    }

    fun removeBlood(blood: Double) {
        owner?.let { BloodChangeEvents.ON_BLOOD_REMOVE.invoker().onRemove(it, blood) }
        absoluteBloodLevel = max(absoluteBloodLevel - blood, 0.0)
    }

    fun addBlood(blood: Double) {
        owner?.let { BloodChangeEvents.ON_BLOOD_ADD.invoker().onAdd(it, blood) }
        absoluteBloodLevel = min(absoluteBloodLevel + blood, 20.0)
    }

    fun feed(entity: LivingEntity, player: PlayerEntity): ActionResult {
        if (getBloodLevel() > 8.5 && lastFed >= player.world.time - getFeedCooldown(player.world))
            return if (entity.isSleeping) ActionResult.FAIL else ActionResult.PASS //Prevents accidentally waking up villagers

        if (goodBloodTag.contains(entity.type)) {
            feed(0.8, entity, player)
            return ActionResult.SUCCESS // I'd like these to be CONSUME but then nothing's sent to the server
        }
        if (mediumBloodTag.contains(entity.type)) {
            feed(0.4, entity, player)
            return ActionResult.SUCCESS
        }
        if (poorBloodTag.contains(entity.type)) {
            feed(0.1, entity, player)
            return ActionResult.SUCCESS
        }
        return ActionResult.PASS
    }

    private fun feed(amount: Double, entity: LivingEntity, player: PlayerEntity) {
        (player.hungerManager as VampireBloodManager).addBlood(amount)
        lastFed = player.world.time
        if (entity is PlayerEntity && entity is Vampirable && entity.isVampire && entity.hungerManager is VampireBloodManager) {
            ((entity.hungerManager) as VampireBloodManager).removeBlood(amount)
        } else {
            entity.damage(BloodLossDamageSource.instance, 1f)
        }
        player.playSound(SoundEvents.ENTITY_GENERIC_DRINK, 1f, 1f)
        val towards = player.pos.subtract(entity.pos).normalize().multiply(0.1)
        for (i in 0..20) {
            val vel = towards.multiply(i.toDouble())
            player.world.addParticle(DustParticleEffect.RED, entity.x+player.random.nextDouble()-0.5, entity.y+player.random.nextDouble(), entity.z+player.random.nextDouble()-0.5, vel.x, vel.y, vel.z)
        }
        BloodDrinkingEvents.ON_BLOOD_DRINK.invoker().onDrink(player, entity, player.world)
    }

    private fun sync(player: PlayerEntity) {
        val buf = PacketByteBuf(Unpooled.buffer())
        buf.writeDouble(absoluteBloodLevel)
        buf.writeLong(lastFed)
        ServerPlayNetworking.send(player as ServerPlayerEntity, bloodLevelPacket, buf)
    }

    private fun heal(player: PlayerEntity) {
        removeBlood((1.05 - (getBloodLevel() / 20.0).pow(2)))
        player.heal(1.0f)
    }
}
