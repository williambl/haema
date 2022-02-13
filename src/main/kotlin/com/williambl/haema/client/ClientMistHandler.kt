package com.williambl.haema.client

import com.williambl.haema.ability.AbilityModule.MIST_FORM
import com.williambl.haema.ability.component.mist_form.MistFormAbilityComponent
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.id
import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking
import net.minecraft.client.MinecraftClient
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.network.PacketByteBuf

object ClientMistHandler: ClientTickEvents.StartTick {
    private var wasMistKeyPressedLast = false
    private var wasExpandKeyPressedLast = false

    override fun onStartTick(client: MinecraftClient) {
        val player = client.player ?: return
        if (HaemaClient.MIST_KEY.isPressed && !wasMistKeyPressedLast && canMist(player)) {
            val buf = PacketByteBuf(Unpooled.buffer())
            ClientPlayNetworking.send(id("mist_form"), buf)
        }
        this.wasMistKeyPressedLast = HaemaClient.MIST_KEY.isPressed

        if (HaemaClient.EXPAND_MIST_KEY.isPressed && !wasExpandKeyPressedLast && canMist(player)) {
            val buf = PacketByteBuf(Unpooled.buffer())
            ClientPlayNetworking.send(id("expand_mist_form"), buf)
        }
        this.wasExpandKeyPressedLast = HaemaClient.EXPAND_MIST_KEY.isPressed

    }

    fun canMist(player: PlayerEntity): Boolean {
        val abilityLevel: Int = player.getAbilityLevel(MIST_FORM)
        return abilityLevel > 0
    }

    fun canExpandMist(player: PlayerEntity): Boolean {
        return MistFormAbilityComponent.entityKey.get(player).isInMistForm
    }
}