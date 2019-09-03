package com.williambl.haema.common.capability

import com.williambl.haema.Haema
import com.williambl.haema.common.util.hasVampirismCapability
import com.williambl.haema.common.util.syncVampirismCapability
import net.minecraft.entity.Entity
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.util.ResourceLocation
import net.minecraftforge.common.capabilities.CapabilityManager
import net.minecraftforge.event.AttachCapabilitiesEvent
import net.minecraftforge.event.entity.EntityJoinWorldEvent
import net.minecraftforge.event.entity.player.PlayerEvent
import net.minecraftforge.fml.common.gameevent.PlayerEvent.PlayerLoggedInEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.fml.common.gameevent.PlayerEvent.PlayerRespawnEvent

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

        if (!event.isWasDeath)
            vampirism.setBloodLevel(oldVampirism?.getBloodLevel() ?: 0.0f)
        vampirism.setIsVampire(oldVampirism?.isVampire() ?: false)
    }

    @SubscribeEvent
    @JvmStatic
    fun onPlayerRespawn(event: PlayerRespawnEvent) {
        event.player.syncVampirismCapability()
    }

    @SubscribeEvent
    @JvmStatic
    fun onPlayerLogIn(event: PlayerLoggedInEvent) {
        if (event.player.hasVampirismCapability())
            event.player.syncVampirismCapability()
    }
}