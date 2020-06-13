package com.williambl.haema

import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.network.ServerSidePacketRegistry
import net.minecraft.entity.data.DataTracker
import net.minecraft.entity.data.TrackedData
import net.minecraft.entity.data.TrackedDataHandlerRegistry
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.Identifier

interface Vampirable {
    var isVampire: Boolean

    fun checkBloodManager();

    companion object {

        val IS_VAMPIRE: TrackedData<Boolean> = DataTracker.registerData(PlayerEntity::class.java, TrackedDataHandlerRegistry.BOOLEAN)

        fun convert(entity: PlayerEntity) {
            if (!(entity as Vampirable).isVampire) {
                entity.kill()
                entity.isVampire = true
                if (entity is ServerPlayerEntity) {
                    val buf = PacketByteBuf(Unpooled.buffer())
                    buf.writeBoolean(true)
                    ServerSidePacketRegistry.INSTANCE.sendToPlayer(entity, Identifier("haema:vampiresync"), buf)
                }
            }
        }
    }
}