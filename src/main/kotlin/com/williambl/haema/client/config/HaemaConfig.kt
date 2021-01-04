package com.williambl.haema.client.config

import me.sargunvohra.mcmods.autoconfig1u.ConfigData
import me.sargunvohra.mcmods.autoconfig1u.annotation.Config

@Config(name = "haema")
class HaemaConfig : ConfigData {
    @JvmField
    val vampireHudPlacement: HudPlacement = HudPlacement.BOTTOM_RIGHT

    @JvmField
    val vampireShaderEnabled = true
}