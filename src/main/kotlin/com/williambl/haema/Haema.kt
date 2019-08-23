package com.williambl.haema

import com.williambl.haema.common.capability.CapabilityVampirismImpl
import com.williambl.haema.common.capability.ICapabilityVampirism
import com.williambl.haema.common.capability.VampirismStorage
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.FMLInitializationEvent
import net.minecraftforge.fml.common.event.FMLPostInitializationEvent
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent
import net.minecraftforge.fml.common.event.FMLServerStartingEvent
import net.minecraftforge.common.capabilities.CapabilityManager



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
        CapabilityManager.INSTANCE.register(ICapabilityVampirism::class.java, VampirismStorage(), ::CapabilityVampirismImpl)
    }

    @Mod.EventHandler
    fun postInit(event: FMLPostInitializationEvent) {
    }

    @Mod.EventHandler
    fun serverStart(event: FMLServerStartingEvent) {
    }
}