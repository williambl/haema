package com.williambl.haema.compat.flan

import com.williambl.haema.Haema
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("flan")) {
        Haema.LOGGER.info("Flan detected. Trespassers will be prosecuted (and prevented from drinking blood).")
        initFlanIntegration()
    }
}
