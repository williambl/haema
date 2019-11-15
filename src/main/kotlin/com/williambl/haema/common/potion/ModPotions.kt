package com.williambl.haema.common.potion

import net.alexwells.kottle.KotlinEventBusSubscriber
import net.minecraft.potion.Effect
import net.minecraftforge.event.RegistryEvent
import net.minecraftforge.eventbus.api.SubscribeEvent

@KotlinEventBusSubscriber(bus = KotlinEventBusSubscriber.Bus.MOD)
object ModPotions {

    @SubscribeEvent
    fun registerPotions(event: RegistryEvent.Register<Effect>) {
        event.registry.registerAll(
                EffectVampiricWeakness(),
                EffectVampiricStrength()
        )
    }
}