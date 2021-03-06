package com.williambl.haema.abilities

import com.williambl.haema.ritual.RitualTableScreenHandler
import com.williambl.haema.util.raytraceForDash
import net.fabricmc.fabric.api.network.ServerSidePacketRegistry
import net.fabricmc.fabric.api.networking.v1.PacketSender
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.MinecraftServer
import net.minecraft.server.network.ServerPlayNetworkHandler
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.util.Identifier

fun registerAbilities() {
    ServerPlayNetworking.registerGlobalReceiver(Identifier("haema:transferlevels")) { server: MinecraftServer, player: ServerPlayerEntity, networkHandler: ServerPlayNetworkHandler, buf: PacketByteBuf, sender: PacketSender ->
        if (player.currentScreenHandler.syncId == buf.readInt() && player.currentScreenHandler is RitualTableScreenHandler) {
            val amount = buf.readInt()
            val from = buf.readInt()
            val to = buf.readInt()
            val currentAmountFrom = (player.currentScreenHandler as RitualTableScreenHandler).getProperty(from)
            val currentAmountTo = (player.currentScreenHandler as RitualTableScreenHandler).getProperty(to)

            if (currentAmountFrom-amount >= 0 && currentAmountTo+amount <= VampireAbility.values()[to].maxLevel) {
                player.currentScreenHandler.setProperty(from, currentAmountFrom-amount)
                player.currentScreenHandler.setProperty(to, currentAmountTo+amount)
            }
        }
    }

    ServerSidePacketRegistry.INSTANCE.register(Identifier("haema:dash")) { packetContext, packetByteBuf ->
        val player = packetContext.player
        val world = player.world
        val target = raytraceForDash(player)

        //TODO: don't trust the client
        if (target != null) {
            val rand = world.random
            for (j in 0 until 3) {
                val x: Double = (target.x - player.x) * rand.nextDouble() + player.x - 0.5
                val y: Double = (target.y - player.y) * rand.nextDouble() + player.y + 1
                val z: Double = (target.z - player.z) * rand.nextDouble() + player.z - 0.5
                (world as ServerWorld).spawnParticles(
                    DustParticleEffect.RED,
                    x, y, z,
                    10,
                    0.5, 1.0, 0.5,
                    0.0
                )
            }
            world.playSound(
                null,
                target.x,
                target.y,
                target.z,
                SoundEvents.ENTITY_GHAST_SHOOT,
                SoundCategory.PLAYERS,
                1f,
                1.5f
            )
            player.teleport(target.x, target.y, target.z)
        }
    }
}