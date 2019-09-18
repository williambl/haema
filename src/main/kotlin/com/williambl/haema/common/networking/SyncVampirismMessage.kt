package com.williambl.haema.common.networking

import com.williambl.haema.common.capability.ICapabilityVampirism
import com.williambl.haema.common.util.getVampirismCapability
import net.minecraft.client.Minecraft
import net.minecraft.network.PacketBuffer
import net.minecraftforge.fml.LogicalSide
import net.minecraftforge.fml.network.NetworkEvent
import java.util.function.Supplier

class SyncVampirismMessage(var bloodLevel: Float = 0.0f, var isVampire: Boolean = false) {

    constructor(capability: ICapabilityVampirism) : this(capability.getBloodLevel(), capability.isVampire())

    constructor(buf: PacketBuffer): this(buf.readFloat(), buf.readBoolean())

    fun encode(buf: PacketBuffer) {
        buf.writeFloat(bloodLevel)
        buf.writeBoolean(isVampire)
    }

    fun handle(ctx: Supplier<NetworkEvent.Context>) {
        if (ctx.get().direction.receptionSide == LogicalSide.CLIENT) {
            ctx.get().enqueueWork {
                Minecraft.getInstance().player.getVampirismCapability().ifPresent {
                    it.setBloodLevel(this.bloodLevel)
                    it.setIsVampire(this.isVampire)
                }
            }
        }

        ctx.get().packetHandled = true
    }
}