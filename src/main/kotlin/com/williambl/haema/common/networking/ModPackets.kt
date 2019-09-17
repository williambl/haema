package com.williambl.haema.common.networking

import com.williambl.haema.Haema
import net.minecraft.network.PacketBuffer
import net.minecraft.util.ResourceLocation
import net.minecraftforge.fml.common.network.NetworkRegistry
import net.minecraftforge.fml.network.NetworkEvent
import net.minecraftforge.fml.network.NetworkRegistry
import net.minecraftforge.fml.relauncher.Side
import java.util.function.BiConsumer
import java.util.function.Function
import java.util.function.Supplier

object ModPackets {

    val protocolVersion = "1"

    val instance = NetworkRegistry.newSimpleChannel(
            ResourceLocation( Haema.MODID, "main"),
            { -> protocolVersion},
            protocolVersion::equals,
            protocolVersion::equals
    )

    fun registerPackets() {
        var discriminator = 0
        instance.registerMessage(
                discriminator++,
                SyncVampirismMessage::class.java,
                SyncVampirismMessage::encode as BiConsumer<SyncVampirismMessage, PacketBuffer>,
                SyncVampirismMessage::decode as Function<PacketBuffer, SyncVampirismMessage>,
                SyncVampirismMessage::handle as BiConsumer<SyncVampirismMessage, Supplier<NetworkEvent.Context>>
        )
        instance.registerMessage(
                discriminator++,
                SunlightHurtMessage::class.java,
                SunlightHurtMessage::encode as BiConsumer<SunlightHurtMessage, PacketBuffer>,
                SunlightHurtMessage::decode as Function<PacketBuffer, SunlightHurtMessage>,
                SunlightHurtMessage::handle as BiConsumer<SunlightHurtMessage, Supplier<NetworkEvent.Context>>
        )
    }
}