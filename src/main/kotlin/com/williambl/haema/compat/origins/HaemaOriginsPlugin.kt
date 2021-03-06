package com.williambl.haema.compat.origins

import com.williambl.haema.logger
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("origins")) {
        logger.info("Origins detected. Adding the vampire origin powers.")
        registerPowerTypes()
    }
}
