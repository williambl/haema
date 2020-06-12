package com.williambl.haema.client

import com.williambl.haema.VampireBloodManager
import com.williambl.haema.VampireEntity
import com.williambl.haema.bloodLevelPackeId
import net.fabricmc.fabric.api.network.ClientSidePacketRegistry
import net.fabricmc.fabric.api.network.PacketConsumer
import net.minecraft.util.Identifier

fun init() {
    ClientSidePacketRegistry.INSTANCE.register(bloodLevelPackeId) { packetContext, packetByteBuf ->
        (packetContext.player.hungerManager as VampireBloodManager).absoluteBloodLevel = packetByteBuf.readDouble()
    }
}