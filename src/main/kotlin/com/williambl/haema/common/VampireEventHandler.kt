package com.williambl.haema.common

import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.networking.ModPackets
import com.williambl.haema.common.networking.SunlightHurtMessage
import com.williambl.haema.common.util.*
import com.williambl.haema.objectholder.ModPotionHolder
import net.minecraft.entity.EntityLiving
import net.minecraft.entity.EntityLivingBase
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.potion.Potion
import net.minecraft.potion.PotionEffect
import net.minecraft.util.DamageSource
import net.minecraft.util.EnumHand
import net.minecraftforge.event.entity.living.*
import net.minecraftforge.event.entity.player.PlayerInteractEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import kotlin.math.max
import kotlin.math.roundToInt

@Mod.EventBusSubscriber
object VampireEventHandler {

    @SubscribeEvent
    @JvmStatic
    fun vampireLivingEvent(e: LivingEvent.LivingUpdateEvent) {
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity as EntityPlayer).hasVampirismCapability() || !(e.entity as EntityPlayer).isVampire())
            return

        val entity = e.entity as EntityPlayer
        val cap = entity.getVampirismCapability()

        if (!entity.hasPotionEffect(ModPotionHolder.vampiric_weakness)) {
            if (entity.isInSunlight()) {
                entity.giveVampiricWeakness(200, max(cap.getInversePowerMultiplier().roundToInt(), 2))
            } else if (cap.hasAbility(VampireAbilities.WEAKNESS)) {
                entity.giveVampiricWeakness(200, 1)
            }
        }

        if (!entity.hasPotionEffect(ModPotionHolder.vampiric_strength)) {
            if (cap.hasAbility(VampireAbilities.STRENGTH)) {
                entity.giveVampiricStrength(200, cap.getPowerMultiplier().roundToInt())
            }
        }

        if (!entity.hasPotionEffect(ModPotionHolder.invisibility)) {
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
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity as EntityPlayer).hasVampirismCapability() || !((e.entity as EntityPlayer).getVampirismCapability().isVampire()))
            return

        val entity = e.entity as EntityPlayer
        val world = entity.world
        val cap = entity.getVampirismCapability()

        if (entity.hasPotionEffect(ModPotionHolder.vampiric_weakness)) {
            e.isCanceled = true
        } else if (!world.isDaytime) {
            e.amount *= 1.6f * cap.getPowerMultiplier()
        }
        if (entity.hasPotionEffect(ModPotionHolder.vampiric_strength)) {
            e.amount *= 1.2f * (entity.getActivePotionEffect(ModPotionHolder.vampiric_strength)?.amplifier ?: 0)
        }
    }

    @SubscribeEvent
    @JvmStatic
    fun vampireDrainBloodRightClickEvent(e: PlayerInteractEvent.EntityInteract) {
        if (
                e.entityPlayer.world.isRemote
                || e.hand != EnumHand.MAIN_HAND
                || !(e.entityPlayer.hasCapability(VampirismProvider.vampirism!!, null))
                || !(e.entityPlayer.getCapability(VampirismProvider.vampirism, null)!!.isVampire())
                || e.target !is EntityLivingBase
        )
            return

        val entityPlayer = e.entityPlayer
        val target = e.target as EntityLivingBase

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
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity as EntityPlayer).hasVampirismCapability() || !((e.entity as EntityPlayer).getVampirismCapability().isVampire()))
            return
        if ((e.entity as EntityPlayer).getVampirismCapability().hasAbility(VampireAbilities.FLIGHT))
            e.isCanceled = true
    }

    @SubscribeEvent
    @JvmStatic
    fun vampireHurtEvent(e: LivingHurtEvent) {
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity as EntityPlayer).hasVampirismCapability() || !((e.entity as EntityPlayer).getVampirismCapability().isVampire()))
            return

        val entity = e.entity as EntityPlayer
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
            ModPackets.instance.sendToAllTracking(SunlightHurtMessage(entity.uniqueID), entity)
            ModPackets.instance.sendTo(SunlightHurtMessage(entity.uniqueID), entity as EntityPlayerMP?)
        }

        entity.addBlood((-0.05 * e.amount).toFloat())

    }

    @SubscribeEvent
    @JvmStatic
    fun cancelMobTargetingEvent(e: LivingSetAttackTargetEvent) {
        if (e.entity.world.isRemote || e.target == null || e.target !is EntityPlayer || !(e.target as EntityPlayer).hasVampirismCapability() || !((e.target as EntityPlayer).getVampirismCapability().isVampire()))
            return

        if ((e.target as EntityPlayer).getVampirismCapability().hasAbility(VampireAbilities.CHARISMA)) {
            if (e.entity.world.rand.nextBoolean()) {
                (e.entityLiving as EntityLiving).attackTarget = null
            }
        }
    }

}