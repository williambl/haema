package com.williambl.haema.compat.flan

import com.williambl.haema.logger
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("flan")) {
        logger.info("Flan detected.")
        initFlanIntegration()
    }
}
