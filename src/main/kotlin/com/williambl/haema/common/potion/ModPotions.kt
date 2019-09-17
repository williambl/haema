package com.williambl.haema.common.potion

import net.alexwells.kottle.KotlinEventBusSubscriber
import net.minecraft.potion.Effect
import net.minecraftforge.event.RegistryEvent
import net.minecraftforge.eventbus.api.SubscribeEvent

@KotlinEventBusSubscriber
object ModPotions {

    @SubscribeEvent
    @JvmStatic
    fun registerPotions(event: RegistryEvent.Register<Effect>) {
        event.registry.registerAll(
                EffectVampiricWeakness(),
                EffectVampiricStrength()
        )
    }
}