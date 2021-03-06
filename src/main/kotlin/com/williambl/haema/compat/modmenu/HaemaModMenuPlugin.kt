package com.williambl.haema.compat.modmenu

import io.github.prospector.modmenu.api.ConfigScreenFactory
import io.github.prospector.modmenu.api.ModMenuApi

class HaemaModMenuPlugin : ModMenuApi {
    override fun getModConfigScreenFactory(): ConfigScreenFactory<*> {
        return ConfigScreenFactory { parent -> HaemaMainConfigScreen(parent) }
    }
}
