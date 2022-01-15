package com.williambl.haema.client

import com.williambl.haema.ability.AbilityModule.DASH
import com.williambl.haema.ability.AbilityModule.MIST_FORM
import com.williambl.haema.client.HaemaClient.dashCooldownValue
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.id
import com.williambl.haema.util.raytraceForDash
import com.williambl.haema.vampireComponent
import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking
import net.minecraft.client.MinecraftClient
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.util.math.Vec3f
import kotlin.math.ln
import kotlin.math.max

object ClientMistHandler: ClientTickEvents.StartTick {
    private var wasPressedLast = false

    override fun onStartTick(client: MinecraftClient) {
        val player = client.player ?: return
        if (HaemaClient.MIST_KEY.isPressed && !wasPressedLast && canMist(player)) {
            val buf = PacketByteBuf(Unpooled.buffer())
            ClientPlayNetworking.send(id("mist_form"), buf)
        }
        this.wasPressedLast = HaemaClient.MIST_KEY.isPressed
    }

    fun canMist(player: PlayerEntity): Boolean {
        val abilityLevel: Int = player.getAbilityLevel(MIST_FORM)
        return abilityLevel > 0
    }
}