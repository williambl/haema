package com.williambl.haema.effect

import com.williambl.haema.id
import net.fabricmc.api.ModInitializer
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.util.registry.Registry

object EffectsModule: ModInitializer {
    val SUNLIGHT_SICKNESS: StatusEffect = Registry.register(
        Registry.STATUS_EFFECT,
        id("sunlight_sickness"),
        SunlightSicknessEffect.instance
    )
    val VAMPIRIC_STRENGTH: StatusEffect = Registry.register(
        Registry.STATUS_EFFECT,
        id("vampiric_strength"),
        VampiricStrengthEffect.instance
    )
    val VAMPIRIC_WEAKNESS: StatusEffect = Registry.register(
        Registry.STATUS_EFFECT,
        id("vampiric_weakness"),
        VampiricWeaknessEffect.instance
    )

    override fun onInitialize() {
    }
}