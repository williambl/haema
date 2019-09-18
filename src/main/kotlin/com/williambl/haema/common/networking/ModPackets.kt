package com.williambl.haema.common.networking

import com.williambl.haema.Haema
import net.minecraft.util.ResourceLocation
import net.minecraftforge.fml.network.NetworkRegistry

object ModPackets {

    val protocolVersion = "1"

    val instance = NetworkRegistry.newSimpleChannel(
            ResourceLocation( Haema.MODID, "main"),
            { -> protocolVersion},
            protocolVersion::equals,
            protocolVersion::equals
    )

    @Suppress("INACCESSIBLE_TYPE")
    fun registerPackets() {
        var discriminator = 0
        instance.registerMessage(
                discriminator++,
                SyncVampirismMessage::class.java,
                { syncVampirismMessage, packetBuffer -> syncVampirismMessage.encode(packetBuffer) },
                { packetBuffer -> SyncVampirismMessage(packetBuffer) },
                { syncVampirismMessage, supplier -> syncVampirismMessage.handle(supplier) }
        )
        instance.registerMessage(
                discriminator++,
                SunlightHurtMessage::class.java,
                { sunlightHurtMessage, packetBuffer -> sunlightHurtMessage.encode(packetBuffer) },
                { packetBuffer -> SunlightHurtMessage(packetBuffer) },
                { sunlightHurtMessage, supplier -> sunlightHurtMessage.handle(supplier) }
        )
    }
}