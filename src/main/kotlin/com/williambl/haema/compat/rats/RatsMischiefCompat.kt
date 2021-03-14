package com.williambl.haema.compat.rats

import com.williambl.haema.logger
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("ratsmischief")) {
        logger.info("Rat's Mischief detected.")
        initRatsMischiefIntegration()
    }
}

fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
    if (FabricLoader.getInstance().isModLoaded("ratsmischief")) {
        registerRatVampireComponent(registry)
    }
}