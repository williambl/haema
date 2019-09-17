package com.williambl.haema.common.networking

import net.minecraft.client.Minecraft
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.network.PacketBuffer
import net.minecraft.particles.ParticleTypes
import net.minecraftforge.fml.LogicalSide
import net.minecraftforge.fml.network.NetworkEvent
import java.util.*
import java.util.function.Supplier

class SunlightHurtMessage(var uuid: UUID = UUID.fromString("00000000-0000-0000-0000-000000000000")) {

    constructor(player: PlayerEntity): this(player.uniqueID)

    fun encode(buf: PacketBuffer) {
        buf.writeString(uuid.toString())
    }

    fun decode(buf: PacketBuffer) {
        uuid = UUID.fromString(buf.readString(64))
    }

        fun handle(ctx: Supplier<NetworkEvent.Context>) {
            if (ctx.get().direction.receptionSide == LogicalSide.CLIENT) {
                ctx.get().enqueueWork {
                    val world = Minecraft.getInstance().world
                    val entity = world.getPlayerByUuid(this.uuid) ?: return@enqueueWork

                    for (i in 0..10) {
                        world.addParticle(
                                ParticleTypes.FLAME,
                                entity.posX + world.rand.nextDouble() - 0.5,
                                entity.posY + world.rand.nextDouble() * 2,
                                entity.posZ + world.rand.nextDouble() - 0.5,
                                0.0,
                                0.1,
                                0.0)
                    }
                }
            }

            ctx.get().packetHandled = true
        }

}