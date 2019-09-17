package com.williambl.haema

import com.williambl.haema.common.capability.CapabilityHandler
import com.williambl.haema.common.networking.ModPackets
import net.alexwells.kottle.KotlinEventBusSubscriber
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.FMLInitializationEvent
import net.minecraftforge.fml.common.event.FMLPostInitializationEvent
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent
import net.minecraftforge.fml.common.event.FMLServerStartingEvent
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent
import net.minecraftforge.fml.event.server.FMLServerStartingEvent


@Mod(Haema.MODID)
@KotlinEventBusSubscriber(modid = Haema.MODID, bus = KotlinEventBusSubscriber.Bus.MOD)
object Haema {

    const val MODID = "haema"
    const val NAME = "Haema"
    const val VERSION = "1.0.0"

    @SubscribeEvent
    fun setup(event: FMLCommonSetupEvent) {
        CapabilityHandler.registerCapabilities()
        ModPackets.registerPackets()
    }
    @SubscribeEvent
    fun clientSetup(event: FMLClientSetupEvent) {
    }

    @SubscribeEvent
    fun serverStart(event: FMLServerStartingEvent) {
    }

}