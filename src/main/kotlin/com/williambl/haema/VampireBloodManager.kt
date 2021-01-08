package com.williambl.haema

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes
import com.williambl.haema.damagesource.BloodLossDamageSource
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.effect.VampiricWeaknessEffect
import com.williambl.haema.util.computeValueWithout
import com.williambl.haema.util.feedCooldown
import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.network.ServerSidePacketRegistry
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.passive.VillagerEntity
import net.minecraft.entity.player.HungerManager
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundTag
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundEvents
import net.minecraft.util.ActionResult
import net.minecraft.village.VillageGossipType
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
        private val VAMPIRE_REACH = EntityAttributeModifier(VAMPIRE_REACH_UUID, "Vampire reach extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
        private val VAMPIRE_ATTACK_RANGE = EntityAttributeModifier(VAMPIRE_ATTACK_RANGE_UUID, "Vampire attack range extension", 2.0, EntityAttributeModifier.Operation.ADDITION)

        fun getFeedCooldown(world: World): Int = world.gameRules[feedCooldown].get()
    }

    var owner: PlayerEntity? = null

    @Deprecated("use getBloodLevel()")
    var absoluteBloodLevel: Double = 7.0

    var lastFed: Long = -24000

    override fun update(player: PlayerEntity?) {
        owner = player!!

        player.isSilent = (getBloodLevel() >= 10 && player.isSprinting) || getBloodLevel() >= 12

        if (getBloodLevel() <= 3) {
            player.addStatusEffect(StatusEffectInstance(VampiricWeaknessEffect.instance, 5, 3 - getBloodLevel().roundToInt()))
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


        if (getBloodLevel() >= 10) {
            player.addStatusEffect(StatusEffectInstance(VampiricStrengthEffect.instance, 10, when {
                getBloodLevel() >= 19 -> 2
                getBloodLevel() >= 14 -> 1
                else -> 0
            }))
        }

        if (getBloodLevel() >= 20) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.INVISIBILITY, 10, 0))
        }

        //Healing at the bottom, so that the health boosts aren't wiped
        if (getBloodLevel() >= 8 || (getBloodLevel() > 0 && player.health <= 0 && player.isAlive)) {
            if (player.world.gameRules.get(GameRules.NATURAL_REGENERATION).get() && player.canFoodHeal()) {
                heal(player, 1.0f)
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
    @Deprecated("use blood", ReplaceWith("getBloodLevel().roundToInt()", "kotlin.math.roundToInt"))
    override fun getFoodLevel(): Int {
        return getBloodLevel().roundToInt()
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

    fun getBloodLevel(): Double {
        return if (owner?.isCreative == true) 20.0 else 20.0 * (sin((absoluteBloodLevel * PI) / 40.0))
    }

    fun removeBlood(blood: Double) {
        absoluteBloodLevel = max(absoluteBloodLevel - blood, 0.0)
    }

    fun addBlood(blood: Double) {
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
        entity.damage(BloodLossDamageSource.instance, 1f)
        player.playSound(SoundEvents.ENTITY_GENERIC_DRINK, 1f, 1f)
        val towards = player.pos.subtract(entity.pos).normalize().multiply(0.1)
        for (i in 0..20) {
            val vel = towards.multiply(i.toDouble())
            player.world.addParticle(DustParticleEffect.RED, entity.x+player.random.nextDouble()-0.5, entity.y+player.random.nextDouble(), entity.z+player.random.nextDouble()-0.5, vel.x, vel.y, vel.z)
        }
        if (entity is VillagerEntity && !entity.isSleeping) {
            entity.gossip.startGossip(player.uuid, VillageGossipType.MAJOR_NEGATIVE, 20)
            if (player.world is ServerWorld)
                vampireHunterSpawner.trySpawnNear(player.world as ServerWorld, player.random, player.blockPos)
        }
    }

    private fun sync(player: PlayerEntity) {
        val buf = PacketByteBuf(Unpooled.buffer())
        buf.writeDouble(absoluteBloodLevel)
        buf.writeLong(lastFed)
        ServerSidePacketRegistry.INSTANCE.sendToPlayer(player, bloodLevelPackeId, buf)
    }

    fun heal(player: PlayerEntity, amount: Float) {
        player.heal(amount)
        if (player.health < player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)?.computeValueWithout(UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92")) ?: 20.0)
            removeBlood((1.05-(getBloodLevel()/20.0).pow(2))*amount)
    }
}
