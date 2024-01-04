package com.williambl.haema.compat.rats

import com.williambl.haema.Haema
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("ratsmischief")) {
        // TODO: No rats version for 1.20.1 yet
        Haema.LOGGER.error("Rat's Mischief is installed, but it's not VampiRat time yet...")
        //Haema.LOGGER.info("Rat's Mischief is installed, it's VampiRat time!")
        //initRatsMischiefIntegration()
    }
}

fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
    if (FabricLoader.getInstance().isModLoaded("ratsmischief")) {
        // TODO: No rats version for 1.20.1 yet
        //registerRatVampireComponent(registry)
    }
}