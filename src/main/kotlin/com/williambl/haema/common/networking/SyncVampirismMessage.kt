package com.williambl.haema.common.networking

import com.williambl.haema.common.capability.ICapabilityVampirism
import com.williambl.haema.common.util.getVampirismCapability
import com.williambl.haema.common.util.hasVampirismCapability
import net.minecraft.client.Minecraft
import net.minecraft.network.PacketBuffer
import net.minecraftforge.fml.LogicalSide
import net.minecraftforge.fml.common.network.simpleimpl.IMessage
import net.minecraftforge.fml.common.network.simpleimpl.IMessageHandler
import net.minecraftforge.fml.common.network.simpleimpl.MessageContext
import net.minecraftforge.fml.network.NetworkEvent
import net.minecraftforge.fml.relauncher.Side
import java.util.function.Supplier

class SyncVampirismMessage(var bloodLevel: Float = 0.0f, var isVampire: Boolean = false) {

    constructor(capability: ICapabilityVampirism) : this(capability.getBloodLevel(), capability.isVampire())

    fun encode(buf: PacketBuffer) {

        buf.writeFloat(bloodLevel)
        buf.writeBoolean(isVampire)
    }

    fun decode(buf: PacketBuffer) {

        bloodLevel = buf.readFloat()
        isVampire = buf.readBoolean()
    }

    fun handle(ctx: Supplier<NetworkEvent.Context>) {
        if (ctx.get().direction.receptionSide == LogicalSide.CLIENT) {
            ctx.get().enqueueWork {
                if (Minecraft.getInstance().player.hasVampirismCapability()) {
                    Minecraft.getInstance().player.getVampirismCapability().let {
                        it.setBloodLevel(this.bloodLevel)
                        it.setIsVampire(this.isVampire)
                    }
                }
            }
        }

        ctx.get().packetHandled = true
    }
}