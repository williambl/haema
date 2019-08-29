package com.williambl.haema.common.networking

import com.williambl.haema.common.capability.ICapabilityVampirism
import com.williambl.haema.common.capability.VampirismProvider
import io.netty.buffer.ByteBuf
import net.minecraft.client.Minecraft
import net.minecraft.network.PacketBuffer
import net.minecraftforge.fml.common.network.simpleimpl.IMessage
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext
import net.minecraftforge.fml.relauncher.Side

class SyncVampirismMessage(var bloodLevel: Float = 0.0f, var isVampire: Boolean = false) : IMessage {

    constructor(capability: ICapabilityVampirism): this(capability.getBloodLevel(), capability.isVampire())

    override fun toBytes(buf: ByteBuf?) {
        val packetBuffer = PacketBuffer(buf!!)

        packetBuffer.writeFloat(bloodLevel)
        packetBuffer.writeBoolean(isVampire)
    }

    override fun fromBytes(buf: ByteBuf?) {
        val packetBuffer = PacketBuffer(buf!!)

        bloodLevel = packetBuffer.readFloat()
        isVampire = packetBuffer.readBoolean()
    }

    class SyncVampirismMessageHandler : IMessageHandler<SyncVampirismMessage, IMessage> {
        override fun onMessage(message: SyncVampirismMessage?, ctx: MessageContext?): IMessage? {
            if (message == null)
                return null

            if (ctx?.side == Side.CLIENT) {
                Minecraft.getMinecraft().player.getCapability(VampirismProvider.vampirism!!, null)?.let {
                    it.setBloodLevel(message.bloodLevel)
                    it.setIsVampire(message.isVampire)
                    println(it.isVampire())
                }
            }

            return null
        }

    }
}