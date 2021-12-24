package com.williambl.haema.compat.rats

import com.williambl.haema.Haema
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("ratsmischief")) {
        Haema.LOGGER.info("Rat's Mischief is installed, it's VampiRat time!")
        initRatsMischiefIntegration()
    }
}

fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
    if (FabricLoader.getInstance().isModLoaded("ratsmischief")) {
        registerRatVampireComponent(registry)
    }
}