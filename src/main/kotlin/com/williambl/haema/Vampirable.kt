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
                entity.isVampire = true
                (entity.hungerManager as VampireBloodManager).absoluteBloodLevel = 3.0
                entity.health = 1f
            }
        }
    }
}
