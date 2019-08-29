package com.williambl.haema

import com.williambl.haema.common.capability.CapabilityHandler
import com.williambl.haema.common.networking.ModPackets
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.FMLInitializationEvent
import net.minecraftforge.fml.common.event.FMLPostInitializationEvent
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent
import net.minecraftforge.fml.common.event.FMLServerStartingEvent


@Mod(modLanguageAdapter = "net.shadowfacts.forgelin.KotlinAdapter", modid = Haema.MODID,
        name = Haema.NAME, version = Haema.VERSION, dependencies = "required-after:forgelin")
object Haema {

    const val MODID = "haema"
    const val NAME = "Haema"
    const val VERSION = "1.0.0"

    @Mod.EventHandler
    fun preInit(event: FMLPreInitializationEvent) {
    }

    @Mod.EventHandler
    fun init(event: FMLInitializationEvent) {
        CapabilityHandler.registerCapabilities()
        ModPackets.registerPackets()
    }

    @Mod.EventHandler
    fun postInit(event: FMLPostInitializationEvent) {
    }

    @Mod.EventHandler
    fun serverStart(event: FMLServerStartingEvent) {
    }

}