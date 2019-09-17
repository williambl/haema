package com.williambl.haema.common

import com.williambl.haema.common.networking.ModPackets
import com.williambl.haema.common.networking.SunlightHurtMessage
import com.williambl.haema.common.util.*
import com.williambl.haema.objectholder.ModPotionHolder
import net.minecraft.entity.EntityLiving
import net.minecraft.entity.EntityLivingBase
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.player.PlayerEntityMP
import net.minecraft.entity.player.ServerPlayerEntity
import net.minecraft.potion.Potion
import net.minecraft.potion.PotionEffect
import net.minecraft.util.DamageSource
import net.minecraft.util.EnumHand
import net.minecraft.util.Hand
import net.minecraftforge.event.entity.living.*
import net.minecraftforge.event.entity.player.PlayerInteractEvent
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.fml.network.PacketDistributor
import kotlin.math.max

@Mod.EventBusSubscriber
object VampireEventHandler {

    @SubscribeEvent
    @JvmStatic
    fun vampireLivingEvent(e: LivingEvent.LivingUpdateEvent) {
        if (e.entity.world.isRemote || e.entity !is PlayerEntity || !(e.entity as PlayerEntity).hasVampirismCapability() || !(e.entity as PlayerEntity).isVampire())
            return

        val entity = e.entity as PlayerEntity
        val cap = entity.getVampirismCapability()

        if (!entity.hasEffect(ModPotionHolder.vampiric_weakness)) {
            if (entity.isInSunlight()) {
                entity.giveVampiricWeakness(200, max(cap.getInversePowerMultiplier().roundToInt(), 2))
            } else if (cap.hasAbility(VampireAbilities.WEAKNESS)) {
                entity.giveVampiricWeakness(200, 1)
            }
        }

        if (!entity.hasEffect(ModPotionHolder.vampiric_strength)) {
            if (cap.hasAbility(VampireAbilities.STRENGTH)) {
                entity.giveVampiricStrength(200, cap.getPowerMultiplier().roundToInt())
            }
        }

        if (!entity.hasEffect(ModPotionHolder.invisibility)) {
            if (cap.hasAbility(VampireAbilities.INVISIBILITY)) {
                entity.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("minecraft:invisibility")!!, 200, cap.getPowerMultiplier().roundToInt()))
            }
        }

        if (entity.isInSunlight()) {
            entity.attackEntityFrom(SunlightDamageSource, (1 shl cap.getInversePowerMultiplier().roundToInt()) * 0.1f)
        }
    }

    @SubscribeEvent
    @JvmStatic
    fun vampireHealEvent(e: LivingHealEvent) {
        if (e.entity.world.isRemote || e.entity !is PlayerEntity || !(e.entity as PlayerEntity).hasVampirismCapability() || !((e.entity as PlayerEntity).getVampirismCapability().isVampire()))
            return

        val entity = e.entity as PlayerEntity
        val world = entity.world
        val cap = entity.getVampirismCapability()

        if (entity.hasEffect(ModPotionHolder.vampiric_weakness)) {
            e.isCanceled = true
        } else if (!world.isDaytime) {
            e.amount *= 1.6f * cap.getPowerMultiplier()
        }
        if (entity.hasEffect(ModPotionHolder.vampiric_strength)) {
            e.amount *= 1.2f * (entity.getActivePotionEffect(ModPotionHolder.vampiric_strength)?.amplifier ?: 0)
        }
    }

    @SubscribeEvent
    @JvmStatic
    fun vampireDrainBloodRightClickEvent(e: PlayerInteractEvent.EntityInteract) {
        if (
                e.entityPlayer.world.isRemote
                || e.hand != Hand.MAIN_HAND
                || !(e.entityPlayer.hasVampirismCapability())
                || !(e.entityPlayer.getVampirismCapability().orElseThrow(::NullPointerException).isVampire())
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
    @JvmStatic
    fun vampireFallEvent(e: LivingFallEvent) {
        if (e.entity.world.isRemote || e.entity !is PlayerEntity || !(e.entity as PlayerEntity).hasVampirismCapability() || !((e.entity as PlayerEntity).getVampirismCapability().isVampire()))
            return
        if ((e.entity as PlayerEntity).getVampirismCapability().hasAbility(VampireAbilities.FLIGHT))
            e.isCanceled = true
    }

    @SubscribeEvent
    @JvmStatic
    fun vampireHurtEvent(e: LivingHurtEvent) {
        if (e.entity.world.isRemote || e.entity !is PlayerEntity || !(e.entity as PlayerEntity).hasVampirismCapability() || !((e.entity as PlayerEntity).getVampirismCapability().isVampire()))
            return

        val entity = e.entity as PlayerEntity
        val source = e.source
        val cap = entity.getVampirismCapability()

        if (source.isFireDamage)
            e.amount *= (cap.getInversePowerMultiplier()/5)+1

        if (source.trueSource is EntityLivingBase) {
            if ((source.trueSource as EntityLivingBase).heldItemMainhand.item in antiVampireItems) {
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
    @JvmStatic
    fun cancelMobTargetingEvent(e: LivingSetAttackTargetEvent) {
        if (e.entity.world.isRemote || e.target == null || e.target !is PlayerEntity || !(e.target as PlayerEntity).hasVampirismCapability() || !((e.target as PlayerEntity).getVampirismCapability().isVampire()))
            return

        if ((e.target as PlayerEntity).getVampirismCapability().hasAbility(VampireAbilities.CHARISMA)) {
            if (e.entity.world.rand.nextBoolean()) {
                (e.entityLiving as LivingEntity).attackTarget = null
            }
        }
    }

}