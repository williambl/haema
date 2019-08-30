package com.williambl.haema.common

import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.util.addBlood
import com.williambl.haema.common.util.giveVampiricWeakness
import net.minecraft.entity.EntityLivingBase
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.Items
import net.minecraft.potion.Potion
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
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity.hasCapability(VampirismProvider.vampirism!!, null)) || !(e.entity.getCapability(VampirismProvider.vampirism, null)!!.isVampire()))
            return

        val entity = e.entity as EntityPlayer
        val world = entity.world
        val cap = entity.getCapability(VampirismProvider.vampirism, null)!!

        if (entity.getActivePotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!) == null) {
            if (world.isDaytime && world.canSeeSky(BlockPos(entity.posX, entity.posY + entity.eyeHeight, entity.posZ))) {
                entity.giveVampiricWeakness(200, cap.getInversePowerMultiplier().roundToInt())
            }
        }

    }

    @SubscribeEvent
    @JvmStatic
    fun vampireHealEvent(e: LivingHealEvent) {
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity.hasCapability(VampirismProvider.vampirism!!, null)) || !(e.entity.getCapability(VampirismProvider.vampirism, null)!!.isVampire()))
            return

        val entity = e.entity as EntityPlayer
        val world = entity.world
        val cap = entity.getCapability(VampirismProvider.vampirism, null)!!

        if (entity.getActivePotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!) != null) {
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
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity.hasCapability(VampirismProvider.vampirism!!, null)) || !(e.entity.getCapability(VampirismProvider.vampirism, null)!!.isVampire()))
            return

        val entity = e.entity as EntityPlayer
        val source = e.source
        val cap = entity.getCapability(VampirismProvider.vampirism, null)!!

        if (source.isFireDamage)
            e.amount *= (cap.getInversePowerMultiplier()/5)+1

        if (source.trueSource is EntityLivingBase) {
            if ((source.trueSource as EntityLivingBase).heldItemMainhand.item == Items.WOODEN_SWORD) {
                e.amount *= (cap.getInversePowerMultiplier()/2)+1
            }
        }

    }

}