package com.williambl.haema.common

import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.util.*
import net.minecraft.entity.EntityLivingBase
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.Items
import net.minecraft.potion.Potion
import net.minecraft.potion.PotionEffect
import net.minecraft.util.DamageSource
import net.minecraft.util.EnumHand
import net.minecraft.util.math.BlockPos
import net.minecraftforge.event.entity.living.LivingEvent
import net.minecraftforge.event.entity.living.LivingHealEvent
import net.minecraftforge.event.entity.living.LivingHurtEvent
import net.minecraftforge.event.entity.player.PlayerInteractEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import kotlin.math.roundToInt

@Mod.EventBusSubscriber
object VampireEventHandler {

    @SubscribeEvent
    @JvmStatic
    fun vampireLivingEvent(e: LivingEvent.LivingUpdateEvent) {
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity as EntityPlayer).hasVampirismCapability() || !(e.entity as EntityPlayer).isVampire())
            return

        val entity = e.entity as EntityPlayer
        val world = entity.world
        val cap = entity.getVampirismCapability()

        if (!entity.hasPotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!)) {
            if (world.isDaytime && world.canSeeSky(BlockPos(entity.posX, entity.posY + entity.eyeHeight, entity.posZ))) {
                entity.giveVampiricWeakness(200, cap.getInversePowerMultiplier().roundToInt())
            } else if ((cap.getAbilities() and VampireAbilities.WEAKNESS.flag) != 0) {
                entity.giveVampiricWeakness(200, 1)
            }
        }

        if (!entity.hasPotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_strength")!!)) {
            if ((cap.getAbilities() and VampireAbilities.STRENGTH.flag) != 0) {
                entity.giveVampiricStrength(200, cap.getPowerMultiplier().roundToInt())
            }
        }

        if (!entity.hasPotionEffect(Potion.getPotionFromResourceLocation("minecraft:night_vision")!!)) {
            if ((cap.getAbilities() and VampireAbilities.VISION.flag) != 0) {
                entity.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("minecraft:night_vision")!!, 200, cap.getPowerMultiplier().roundToInt()))
            }
        }

        if (!entity.hasPotionEffect(Potion.getPotionFromResourceLocation("minecraft:invisibility")!!)) {
            if ((cap.getAbilities() and VampireAbilities.INVISIBILITY.flag) != 0) {
                entity.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("minecraft:invisibility")!!, 200, cap.getPowerMultiplier().roundToInt()))
            }
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

        if (entity.hasPotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!)) {
            e.isCanceled = true
        } else if (!world.isDaytime) {
            e.amount *= 1.6f * cap.getPowerMultiplier()
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
            target.attackEntityFrom(DamageSource.causePlayerDamage(entityPlayer).setDamageBypassesArmor().setMagicDamage(), 0.5f)
            entityPlayer.addBlood(0.01f)
        }

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
            if ((source.trueSource as EntityLivingBase).heldItemMainhand.item in arrayOf(Items.WOODEN_SWORD, Items.STICK)) {
                e.amount *= (cap.getInversePowerMultiplier()/2)+1
            }
        }

        entity.addBlood((-0.05 * e.amount).toFloat())

    }

}