package com.williambl.haema.common.potion

import net.minecraft.potion.Potion
import net.minecraftforge.event.RegistryEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent

@Mod.EventBusSubscriber()
object ModPotions {

    @SubscribeEvent
    @JvmStatic
    fun registerPotions(event: RegistryEvent.Register<Potion>) {
        event.registry.registerAll(
                PotionVampiricWeakness()
        )
    }
}