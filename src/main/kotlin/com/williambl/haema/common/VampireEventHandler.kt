package com.williambl.haema.common

import com.williambl.haema.common.capability.VampirismProvider
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.potion.Potion
import net.minecraft.potion.PotionEffect
import net.minecraft.util.math.BlockPos
import net.minecraftforge.event.entity.living.LivingEvent
import net.minecraftforge.event.entity.living.LivingHealEvent
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
                entity.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!, 200, cap.getPowerMultiplier().roundToInt()))
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

}