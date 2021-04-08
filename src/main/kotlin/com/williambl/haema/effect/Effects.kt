package com.williambl.haema.effect

import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry

fun registerEffects() {
    Registry.register(
        Registry.STATUS_EFFECT,
        Identifier("haema:sunlight_sickness"),
        SunlightSicknessEffect.instance
    )
    Registry.register(
        Registry.STATUS_EFFECT,
        Identifier("haema:vampiric_strength"),
        VampiricStrengthEffect.instance
    )
    Registry.register(
        Registry.STATUS_EFFECT,
        Identifier("haema:vampiric_weakness"),
        VampiricWeaknessEffect.instance
    )
}