package com.williambl.haema.common

import com.williambl.haema.common.networking.ModPackets
import com.williambl.haema.common.networking.SunlightHurtMessage
import com.williambl.haema.common.util.*
import com.williambl.haema.objectholder.ModEffectHolder
import net.alexwells.kottle.KotlinEventBusSubscriber
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.MobEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.player.ServerPlayerEntity
import net.minecraft.potion.EffectInstance
import net.minecraft.util.DamageSource
import net.minecraft.util.Hand
import net.minecraftforge.event.entity.living.*
import net.minecraftforge.event.entity.player.PlayerInteractEvent
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.network.PacketDistributor
import kotlin.math.max
import kotlin.math.roundToInt

@KotlinEventBusSubscriber
object VampireEventHandler {

    @SubscribeEvent
    fun vampireLivingEvent(e: LivingEvent.LivingUpdateEvent) {
        if (e.entity.world.isRemote || e.entity !is PlayerEntity || !(e.entity as PlayerEntity).hasVampirismCapability() || !(e.entity as PlayerEntity).isVampire())
            return

        val entity = e.entity as PlayerEntity
        val cap = entity.getVampirismCapabilityOrThrow()

        if (!entity.hasEffect(ModEffectHolder.vampiric_weakness)) {
            if (entity.isInSunlight()) {
                entity.giveVampiricWeakness(200, max(cap.getInversePowerMultiplier().roundToInt(), 2))
            } else if (cap.hasAbility(VampireAbilities.WEAKNESS)) {
                entity.giveVampiricWeakness(200, 1)
            }
        }

        if (!entity.hasEffect(ModEffectHolder.vampiric_strength)) {
            if (cap.hasAbility(VampireAbilities.STRENGTH)) {
                entity.giveVampiricStrength(200, cap.getPowerMultiplier().roundToInt())
            }
        }

        if (!entity.hasEffect(ModEffectHolder.invisibility)) {
            if (cap.hasAbility(VampireAbilities.INVISIBILITY)) {
                entity.addPotionEffect(EffectInstance(ModEffectHolder.invisibility, 200, cap.getPowerMultiplier().roundToInt()))
            }
        }

        if (entity.isInSunlight()) {
            entity.attackEntityFrom(SunlightDamageSource, (1 shl cap.getInversePowerMultiplier().roundToInt()) * 0.1f)
        }
    }

    @SubscribeEvent
    fun vampireHealEvent(e: LivingHealEvent) {
        if (e.entity.world.isRemote || e.entity !is PlayerEntity || !(e.entity as PlayerEntity).hasVampirismCapability() || !((e.entity as PlayerEntity).getVampirismCapabilityOrThrow().isVampire()))
            return

        val entity = e.entity as PlayerEntity
        val world = entity.world
        val cap = entity.getVampirismCapabilityOrThrow()

        if (entity.hasEffect(ModEffectHolder.vampiric_weakness)) {
            e.isCanceled = true
        } else if (!world.isDaytime) {
            e.amount *= 1.6f * cap.getPowerMultiplier()
        }
        if (entity.hasEffect(ModEffectHolder.vampiric_strength)) {
            e.amount *= 1.2f * (entity.getActivePotionEffect(ModEffectHolder.vampiric_strength)?.amplifier ?: 0)
        }
    }

    @SubscribeEvent
    fun vampireDrainBloodRightClickEvent(e: PlayerInteractEvent.EntityInteract) {
        if (
                e.entityPlayer.world.isRemote
                || e.hand != Hand.MAIN_HAND
                || !(e.entityPlayer.hasVampirismCapability())
                || !(e.entityPlayer.getVampirismCapabilityOrThrow().isVampire())
                || e.target !is LivingEntity
        )
            return

        val entityPlayer = e.entityPlayer
        val target = e.target as LivingEntity

        val dist = entityPlayer.positionVector.distanceTo(target.positionVector)

        if (dist > 1.5)
            return

        if (!target.isEntityUndead) {
            target.attackEntityFrom(DamageSource.MAGIC.setDamageBypassesArmor().setMagicDamage(), 0.5f)
            entityPlayer.addBlood(0.01f)
        }

    }

    @SubscribeEvent
    fun vampireFallEvent(e: LivingFallEvent) {
        if (e.entity.world.isRemote || e.entity !is PlayerEntity || !(e.entity as PlayerEntity).hasVampirismCapability() || !((e.entity as PlayerEntity).getVampirismCapabilityOrThrow().isVampire()))
            return
        if ((e.entity as PlayerEntity).getVampirismCapabilityOrThrow().hasAbility(VampireAbilities.FLIGHT))
            e.isCanceled = true
    }

    @SubscribeEvent
    fun vampireHurtEvent(e: LivingHurtEvent) {
        if (e.entity.world.isRemote || e.entity !is PlayerEntity || !(e.entity as PlayerEntity).hasVampirismCapability() || !((e.entity as PlayerEntity).getVampirismCapabilityOrThrow().isVampire()))
            return

        val entity = e.entity as PlayerEntity
        val source = e.source
        val cap = entity.getVampirismCapabilityOrThrow()

        if (source.isFireDamage)
            e.amount *= (cap.getInversePowerMultiplier()/5)+1

        if (source.trueSource is LivingEntity) {
            if ((source.trueSource as LivingEntity).heldItemMainhand.item in antiVampireItems) {
                e.amount *= (cap.getInversePowerMultiplier()/2)+1
            }
        }

        if (source is SunlightDamageSource) {
            ModPackets.instance.send(PacketDistributor.TRACKING_ENTITY.with { entity }, SunlightHurtMessage(entity.uniqueID))
            ModPackets.instance.send(PacketDistributor.PLAYER.with { entity as ServerPlayerEntity }, SunlightHurtMessage(entity.uniqueID))
        }

        entity.addBlood((-0.05 * e.amount).toFloat())

    }

    @SubscribeEvent
    fun cancelMobTargetingEvent(e: LivingSetAttackTargetEvent) {
        if (e.entity.world.isRemote || e.target == null || e.target !is PlayerEntity || !(e.target as PlayerEntity).hasVampirismCapability() || !((e.target as PlayerEntity).getVampirismCapabilityOrThrow().isVampire()))
            return

        if ((e.target as PlayerEntity).getVampirismCapabilityOrThrow().hasAbility(VampireAbilities.CHARISMA)) {
            if (e.entity.world.rand.nextBoolean()) {
                (e.entityLiving as MobEntity).attackTarget = null
            }
        }
    }

}