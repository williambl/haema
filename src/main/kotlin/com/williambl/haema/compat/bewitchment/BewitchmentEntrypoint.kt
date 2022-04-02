package com.williambl.haema.compat.bewitchment

import com.williambl.haema.Haema
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import net.fabricmc.loader.api.FabricLoader

fun init() {
    if (FabricLoader.getInstance().isModLoaded("bewitchment")) {
        Haema.LOGGER.info("Hey Bewitchment, mind if I modify your vampires a little?")
        registerBewitchmentEventListeners()
    }
}

fun clientInit() {
    if (FabricLoader.getInstance().isModLoaded("bewitchment")) {
        Haema.LOGGER.info("Haema's gotta change some Bewitchment client stuff too...")
        registerBewitchmentClientEventListeners()
    }
}

fun entityComponentInit(registry: EntityComponentFactoryRegistry) {
    if (FabricLoader.getInstance().isModLoaded("bewitchment")) {
        Haema.LOGGER.info("Haema's fiddling with Bewitchment's vampires :D")
        registerEntityComponentFactories(registry)
    }
}