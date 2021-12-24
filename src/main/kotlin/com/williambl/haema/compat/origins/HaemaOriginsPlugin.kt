package com.williambl.haema.compat.origins

import com.williambl.haema.Haema
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("origins")) {
        Haema.LOGGER.info("Origins detected. Adding the vampire origin powers...")
        registerPowerTypes()
        registerOriginsCompatEvents()
    }
}
