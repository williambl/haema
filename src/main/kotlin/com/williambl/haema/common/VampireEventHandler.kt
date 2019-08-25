package com.williambl.haema.common

import com.williambl.haema.common.capability.VampirismProvider
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.potion.Potion
import net.minecraft.potion.PotionEffect
import net.minecraft.util.math.BlockPos
import net.minecraftforge.event.entity.living.LivingEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent

@Mod.EventBusSubscriber
object VampireEventHandler {

    @SubscribeEvent
    @JvmStatic
    fun vampireLivingEvent(e: LivingEvent.LivingUpdateEvent) {
        if (e.entity.world.isRemote || e.entity !is EntityPlayer || !(e.entity.hasCapability(VampirismProvider.vampirism!!, null)))
            return

        val entity = e.entity as EntityPlayer
        val world = entity.world

        if (world.rand.nextDouble() > 0.8 && entity.getActivePotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!) == null) {
            if (world.isDaytime && world.canSeeSky(BlockPos(entity.posX, entity.posY + entity.eyeHeight, entity.posZ))) {
                entity.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!, 200, 1))
            }
        }
    }

}