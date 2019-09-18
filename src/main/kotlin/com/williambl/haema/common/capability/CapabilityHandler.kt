package com.williambl.haema.common.capability

import com.williambl.haema.Haema
import com.williambl.haema.common.util.getVampirismCapability
import com.williambl.haema.common.util.hasVampirismCapability
import com.williambl.haema.common.util.syncVampirismCapability
import net.alexwells.kottle.KotlinEventBusSubscriber
import net.minecraft.entity.Entity
import net.minecraft.util.ResourceLocation
import net.minecraftforge.common.capabilities.CapabilityManager
import net.minecraftforge.event.AttachCapabilitiesEvent
import net.minecraftforge.event.entity.player.PlayerEvent
import net.minecraftforge.eventbus.api.SubscribeEvent

@KotlinEventBusSubscriber
object CapabilityHandler {

    val vampirismResourceLocation = ResourceLocation(Haema.MODID, "vampirism")

    fun registerCapabilities() {
        CapabilityManager.INSTANCE.register(ICapabilityVampirism::class.java, VampirismStorage(), ::CapabilityVampirismImpl)
    }

    @SubscribeEvent
    @JvmStatic
    fun attachCapabilities(e: AttachCapabilitiesEvent<Entity>) {
        if (e.`object` is PlayerEvent)
            e.addCapability(vampirismResourceLocation, VampirismProvider())
    }

    @SubscribeEvent
    @JvmStatic
    fun onPlayerClone(event: PlayerEvent.Clone) {
        val player = event.entityPlayer
        val vampirism = player.getVampirismCapability()
        val oldVampirism = event.original.getVampirismCapability()

        oldVampirism.ifPresent { oldCap ->
            vampirism.ifPresent { cap ->
                if (!event.isWasDeath)
                    cap.setBloodLevel(oldCap.getBloodLevel())
                cap.setIsVampire(oldCap.isVampire())
            }
        }
    }

    @SubscribeEvent
    @JvmStatic
    fun onPlayerRespawn(event: PlayerEvent.PlayerRespawnEvent) {
        event.player.syncVampirismCapability()
    }

    @SubscribeEvent
    @JvmStatic
    fun onPlayerLogIn(event: PlayerEvent.PlayerLoggedInEvent) {
        if (event.player.hasVampirismCapability())
            event.player.syncVampirismCapability()
    }
}