package com.williambl.haema.common.networking

import com.williambl.haema.Haema
import net.minecraftforge.fml.common.network.NetworkRegistry
import net.minecraftforge.fml.relauncher.Side

object ModPackets {

    val instance = NetworkRegistry.INSTANCE.newSimpleChannel(Haema.MODID)

    fun registerPackets() {
        var discriminator = 0
        instance.registerMessage(SyncVampirismMessage.SyncVampirismMessageHandler::class.java, SyncVampirismMessage::class.java, discriminator++, Side.CLIENT)
        instance.registerMessage(SunlightHurtMessage.SunlightHurtMessageHandler::class.java, SunlightHurtMessage::class.java, discriminator++, Side.CLIENT)
    }
}