package com.williambl.haema.common.capability

import com.williambl.haema.Haema
import net.minecraft.entity.Entity
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.util.ResourceLocation
import net.minecraftforge.common.capabilities.CapabilityManager
import net.minecraftforge.event.AttachCapabilitiesEvent
import net.minecraftforge.event.entity.player.PlayerEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent

@Mod.EventBusSubscriber
object CapabilityHandler {

    val vampirismResourceLocation = ResourceLocation(Haema.MODID, "vampirism")

    fun registerCapabilities() {
        CapabilityManager.INSTANCE.register(ICapabilityVampirism::class.java, VampirismStorage(), ::CapabilityVampirismImpl)
    }

    @SubscribeEvent
    @JvmStatic
    fun attachCapabilities(e: AttachCapabilitiesEvent<Entity>) {
        if (e.`object` is EntityPlayer)
            e.addCapability(vampirismResourceLocation, VampirismProvider())
    }

    @SubscribeEvent
    @JvmStatic
    fun onPlayerClone(event: PlayerEvent.Clone) {
        val player = event.entityPlayer
        val vampirism = player.getCapability(VampirismProvider.vampirism!!, null)!!
        val oldVampirism = event.original.getCapability(VampirismProvider.vampirism, null)

        vampirism.setBloodLevel(oldVampirism?.getBloodLevel() ?: 0.0f)
        vampirism.setIsVampire(oldVampirism?.isVampire() ?: false)
    }
}