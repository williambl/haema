package com.williambl.haema.client.config

import me.shedaniel.autoconfig.ConfigData
import me.shedaniel.autoconfig.annotation.Config
import me.shedaniel.autoconfig.annotation.ConfigEntry

@Config(name = "haema")
class HaemaConfig : ConfigData {
    @JvmField
    @ConfigEntry.Gui.EnumHandler(option = ConfigEntry.Gui.EnumHandler.EnumDisplayOption.BUTTON)
    val vampireHudPlacement: HudPlacement = HudPlacement.BOTTOM_RIGHT

    @JvmField
    val vampireShaderEnabled = true

    @JvmField
    @ConfigEntry.Gui.Tooltip
    val brightAdjust = 1.0f

    @JvmField
    @ConfigEntry.Gui.Tooltip
    val distortionAdjust = 1.0f

    @JvmField
    @ConfigEntry.Gui.Tooltip
    val saturationAdjust = 1.0f
}