package com.williambl.haema.common.networking

import io.netty.buffer.ByteBuf
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.network.PacketBuffer
import net.minecraft.util.EnumParticleTypes
import net.minecraftforge.fml.common.network.simpleimpl.IMessage
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext
import net.minecraftforge.fml.relauncher.Side
import java.util.*

class SunlightHurtMessage(var uuid: UUID = UUID.fromString("00000000-0000-0000-0000-000000000000")) : IMessage {

    constructor(player: EntityPlayer): this(player.uniqueID)

    override fun toBytes(buf: ByteBuf?) {
        val packetBuffer = PacketBuffer(buf!!)

        packetBuffer.writeString(uuid.toString())
    }

    override fun fromBytes(buf: ByteBuf?) {
        val packetBuffer = PacketBuffer(buf!!)

        uuid = UUID.fromString(packetBuffer.readString(64))
    }

    class SunlightHurtMessageHandler : IMessageHandler<SunlightHurtMessage, IMessage> {
        override fun onMessage(message: SunlightHurtMessage?, ctx: MessageContext?): IMessage? {
            if (message == null)
                return null

            if (ctx?.side == Side.CLIENT) {
                val world = Minecraft.getMinecraft().world
                val entity = world.getPlayerEntityByUUID(message.uuid) ?: return null

                println("received")

                for (i in 0..10) {
                    world.spawnParticle(
                            EnumParticleTypes.FLAME,
                            entity.posX+world.rand.nextDouble()-0.5,
                            entity.posY+world.rand.nextDouble()*2,
                            entity.posZ+world.rand.nextDouble()-0.5,
                            0.0,
                            0.1,
                            0.0)
                }
            }

            return null
        }

    }
}