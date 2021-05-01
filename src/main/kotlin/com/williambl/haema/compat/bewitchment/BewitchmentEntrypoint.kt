package com.williambl.haema.compat.bewitchment

import com.williambl.haema.logger
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("bewitchment")) {
        logger.info("Bewitchment detected.")
        registerBewitchmentEventListeners()
    }
}

fun clientInit() {
    if (FabricLoader.getInstance().isModLoaded("bewitchment")) {
        logger.info("Bewitchment detected.")
        registerBewitchmentClientEventListeners()
    }
}