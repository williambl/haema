package com.williambl.haema.component

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes
import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.VampireAbility
import com.williambl.haema.api.BloodChangeEvents
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.damagesource.BloodLossDamageSource
import com.williambl.haema.effect.SunlightSicknessEffect
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.effect.VampiricWeaknessEffect
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.id
import com.williambl.haema.isVampire
import com.williambl.haema.util.HaemaGameRules
import com.williambl.haema.util.computeValueWithout
import dev.onyxstudios.cca.api.v3.component.CopyableComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import net.fabricmc.fabric.api.tag.TagFactory
import net.minecraft.entity.EntityType
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.nbt.NbtCompound
import net.minecraft.particle.DustParticleEffect
import net.minecraft.sound.SoundEvents
import net.minecraft.tag.Tag
import net.minecraft.util.ActionResult
import net.minecraft.util.Identifier
import net.minecraft.world.GameRules
import net.minecraft.world.World
import java.util.*
import kotlin.math.*
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

//TODO: sync packet
class VampirePlayerComponent(val player: LivingEntity) : VampireComponent, AutoSyncedComponent, CopyableComponent<VampirePlayerComponent> {
    private val syncCallback = { _: KProperty<*>, _: Any?, _: Any? ->
        if (!player.world.isClient) {
            VampireComponent.entityKey.sync(player)
        }
    }

    override var isVampire: Boolean by Delegates.observable(false, syncCallback)
    override var isPermanentVampire: Boolean by Delegates.observable(false, syncCallback)
    override var isKilled: Boolean by Delegates.observable(false, syncCallback)

    override var absoluteBlood: Double by Delegates.observable(7.0, syncCallback)
    override val blood: Double
        get() = if (player is PlayerEntity && player.isCreative) 20.0 else 20.0 * (sin((absoluteBlood * PI) / 40.0))

    override var abilities: MutableMap<VampireAbility, Int> = mutableMapOf(
        AbilityModule.STRENGTH to 1,
        AbilityModule.DASH to 1,
        AbilityModule.INVISIBILITY to 0,
        AbilityModule.IMMORTALITY to 1,
        AbilityModule.VISION to 1
    )

    override var ritualsUsed: MutableSet<Identifier> = mutableSetOf()

    override var lastFed: Long = -24000

    override fun writeToNbt(tag: NbtCompound) {
        tag.putBoolean("isVampire", isVampire)
        tag.putBoolean("isPermanentVampire", isPermanentVampire)
        tag.putBoolean("isKilled", isKilled)
        tag.putDouble("AbsoluteBlood", absoluteBlood)
        tag.put("abilities", NbtCompound().also { abilitiesTag -> abilities.forEach { (ability, value) -> abilitiesTag.putInt(AbilityModule.ABILITY_REGISTRY.getId(ability).toString(), value) } })
        tag.put("ritualsUsed", NbtCompound().also {
            it.putInt("Length", ritualsUsed.size)
            ritualsUsed.forEachIndexed { idx, id -> it.putString(idx.toString(), id.toString()) }
        })
    }

    override fun readFromNbt(tag: NbtCompound) {
        isVampire = tag.getBoolean("isVampire")
        isPermanentVampire = tag.getBoolean("isPermanentVampire")
        isKilled = tag.getBoolean("isKilled")
        absoluteBlood = tag.getDouble("AbsoluteBlood")
        val abilitiesTag = tag.getCompound("abilities")
        abilitiesTag.fixAbilityData()
        AbilityModule.ABILITY_REGISTRY.entries.filter { abilitiesTag.contains(it.key.value.toString()) }.forEach { abilities[it.value] = abilitiesTag.getInt(it.key.value.toString()) }
        val ritualsUsedTag = tag.getCompound("ritualsUsed")
        ritualsUsed = List(ritualsUsedTag.getInt("Length")) { idx -> Identifier(ritualsUsedTag.getString(idx.toString())) }.toMutableSet()
    }

    override fun copyFrom(other: VampirePlayerComponent) {
        isVampire = other.isVampire
        isPermanentVampire = other.isPermanentVampire
        abilities = other.abilities
        ritualsUsed = other.ritualsUsed
    }

    //TODO: move health boost stuff to another component
    override fun serverTick() {
        player.isSilent = (blood >= 10 && player.isSprinting) || blood >= 12

        if (blood > 3 && player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)?.hasModifier(
                VAMPIRE_HEALTH_BOOST
            ) == false) {
            player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)!!.addPersistentModifier(VAMPIRE_HEALTH_BOOST)
        }

        if (blood <= 3) {
            player.addStatusEffect(StatusEffectInstance(VampiricWeaknessEffect.instance, 5, 3 - blood.roundToInt(), false, false, true))
            if (player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)?.hasModifier(VAMPIRE_HEALTH_BOOST) == true) {
                player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)!!.removeModifier(VAMPIRE_HEALTH_BOOST)
            }
        }

        val reachAttr = player.getAttributeInstance(ReachEntityAttributes.REACH)
        val attackRangeAttr = player.getAttributeInstance(ReachEntityAttributes.ATTACK_RANGE)

        if (blood >= 6 && (reachAttr?.hasModifier(VAMPIRE_REACH) == false || attackRangeAttr?.hasModifier(
                VAMPIRE_ATTACK_RANGE
            ) == false)
        ) {
            reachAttr?.addTemporaryModifier(VAMPIRE_REACH)
            attackRangeAttr?.addTemporaryModifier(VAMPIRE_ATTACK_RANGE)
        } else if (reachAttr?.hasModifier(VAMPIRE_REACH) != false || attackRangeAttr?.hasModifier(
                VAMPIRE_ATTACK_RANGE
            ) != false) {
            reachAttr?.removeModifier(VAMPIRE_REACH)
            attackRangeAttr?.removeModifier(VAMPIRE_ATTACK_RANGE)
        }


        if (blood >= 10 && player.getAbilityLevel(AbilityModule.STRENGTH) > 0) {
            player.addStatusEffect(
                StatusEffectInstance(
                    VampiricStrengthEffect.instance, 40, when {
                        blood >= 19 -> 2
                        blood >= 14 -> 1
                        else -> 0
                    }.coerceAtMost((player).getAbilityLevel(AbilityModule.STRENGTH)-1), false, false, true)
            )
        }


        //Healing at the bottom, so that the health boosts aren't wiped
        if (blood >= 8 || (blood > 0 && player.health <= 0 && player.isAlive)) {
            if (player.world.gameRules.get(GameRules.NATURAL_REGENERATION).get() && player.health > 0 && player.health < player.maxHealth) {
                val defaultMaxHealth = player.getAttributeInstance(EntityAttributes.GENERIC_MAX_HEALTH)?.computeValueWithout(
                    UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92")) ?: 20.0
                if (player.health >= defaultMaxHealth) {
                    if (player.age % 20 == 0 && (player.health - defaultMaxHealth) < when {
                            blood >= 19 -> 20
                            blood >= 14 -> 10
                            blood >= 10 -> 6
                            else -> 0
                        }) {
                        heal(player)
                    }
                } else if (!player.hasStatusEffect(SunlightSicknessEffect.instance)) {
                    heal(player)
                }
            } else if (player.health <= 0) {
                player.health = 1f
                removeBlood(1.0)
            }
        }
    }

    override fun removeBlood(blood: Double) {
        player.let { BloodChangeEvents.ON_BLOOD_REMOVE.invoker().onRemove(it, blood) }
        absoluteBlood = max(absoluteBlood - blood, 0.0)
    }

    override fun addBlood(blood: Double) {
        player.let { BloodChangeEvents.ON_BLOOD_ADD.invoker().onAdd(it, blood) }
        absoluteBlood = min(absoluteBlood + blood, 20.0)
    }

    override fun feed(entity: LivingEntity): ActionResult {
        if (blood > 8.5 && lastFed >= player.world.time - getFeedCooldown(player.world))
            return if (entity.isSleeping) ActionResult.FAIL else ActionResult.PASS //Prevents accidentally waking up villagers

        if (goodBloodTag.contains(entity.type)) {
            feed(0.8, entity)
            return ActionResult.SUCCESS // I'd like these to be CONSUME but then nothing's sent to the server
        }
        if (mediumBloodTag.contains(entity.type)) {
            feed(0.4, entity)
            return ActionResult.SUCCESS
        }
        if (poorBloodTag.contains(entity.type)) {
            feed(0.1, entity)
            return ActionResult.SUCCESS
        }
        return ActionResult.PASS
    }

    private fun feed(amount: Double, entity: LivingEntity) {
        addBlood(amount)
        lastFed = player.world.time
        if (entity is PlayerEntity && entity.isVampire) {
            removeBlood(amount)
        } else {
            entity.damage(BloodLossDamageSource.instance, 1f)
        }
        player.playSound(SoundEvents.ENTITY_GENERIC_DRINK, 1f, 1f)
        val towards = player.pos.subtract(entity.pos).normalize().multiply(0.1)
        for (i in 0..20) {
            val vel = towards.multiply(i.toDouble())
            player.world.addParticle(DustParticleEffect.DEFAULT, entity.x+player.random.nextDouble()-0.5, entity.y+player.random.nextDouble(), entity.z+player.random.nextDouble()-0.5, vel.x, vel.y, vel.z)
        }
        BloodDrinkingEvents.ON_BLOOD_DRINK.invoker().onDrink(player, entity, player.world)
    }

    private fun heal(player: LivingEntity) {
        removeBlood((1.05 - (blood / 20.0).pow(2)))
        player.heal(1.0f)
    }

    private fun NbtCompound.fixAbilityData() {
        fun fixAbility(oldName: String, ability: VampireAbility) {
            val newName = AbilityModule.ABILITY_REGISTRY.getId(ability).toString()
            if (this.contains(oldName) && !this.contains(newName)) {
                this.putInt(newName, this.getInt(oldName))
                this.remove(oldName)
            }
        }

        fixAbility("NONE", AbilityModule.STRENGTH)
        fixAbility("STRENGTH", AbilityModule.STRENGTH)
        fixAbility("DASH", AbilityModule.STRENGTH)
        fixAbility("INVISIBILITY", AbilityModule.STRENGTH)
        fixAbility("IMMORTALITY", AbilityModule.STRENGTH)
        fixAbility("VISION", AbilityModule.STRENGTH)
    }

    companion object {
        private val VAMPIRE_REACH_UUID = UUID.fromString("0eb4fc5f-71d5-4440-b517-bcc18e1df6f4")
        private val VAMPIRE_ATTACK_RANGE_UUID = UUID.fromString("3267a46b-2b48-429f-a3a8-439aa87a876d")
        private val VAMPIRE_HEALTH_BOOST_UUID = UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92")
        private val VAMPIRE_REACH = EntityAttributeModifier(VAMPIRE_REACH_UUID, "Vampire reach extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
        private val VAMPIRE_ATTACK_RANGE = EntityAttributeModifier(VAMPIRE_ATTACK_RANGE_UUID, "Vampire attack range extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
        private val VAMPIRE_HEALTH_BOOST = EntityAttributeModifier(VAMPIRE_HEALTH_BOOST_UUID, "Vampire health boost", 1.0, EntityAttributeModifier.Operation.MULTIPLY_BASE)

        val goodBloodTag: Tag<EntityType<*>> = TagFactory.ENTITY_TYPE.create(id("good_blood_sources"))
        val mediumBloodTag: Tag<EntityType<*>> = TagFactory.ENTITY_TYPE.create(id("medium_blood_sources"))
        val poorBloodTag: Tag<EntityType<*>> = TagFactory.ENTITY_TYPE.create(id("poor_blood_sources"))

        fun getFeedCooldown(world: World): Int = world.gameRules[HaemaGameRules.feedCooldown].get()
    }
}