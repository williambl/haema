package com.williambl.haema.client

import com.williambl.haema.ability.AbilityModule.MIST_FORM
import com.williambl.haema.ability.component.mist_form.MistFormAbilityComponent
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.id
import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking
import net.minecraft.client.MinecraftClient
import net.minecraft.client.option.KeyBinding
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.network.PacketByteBuf
import org.lwjgl.glfw.GLFW

object ClientMistHandler: ClientTickEvents.StartTick {
    val MIST_KEY = KeyBinding("key.haema.mist_form", GLFW.GLFW_KEY_K, "key.categories.movement")
    val EXPAND_MIST_KEY = KeyBinding("key.haema.expand_mist_form", GLFW.GLFW_KEY_M, "key.categories.movement")

    private var wasMistKeyPressedLast = false
    private var wasExpandKeyPressedLast = false

    fun init() {
        KeyBindingHelper.registerKeyBinding(MIST_KEY)
        KeyBindingHelper.registerKeyBinding(EXPAND_MIST_KEY)
    }

    override fun onStartTick(client: MinecraftClient) {
        val player = client.player ?: return
        if (MIST_KEY.isPressed && !wasMistKeyPressedLast && canMist(player)) {
            val buf = PacketByteBuf(Unpooled.buffer())
            ClientPlayNetworking.send(id("mist_form"), buf)
        }
        this.wasMistKeyPressedLast = MIST_KEY.isPressed

        if (EXPAND_MIST_KEY.isPressed && !wasExpandKeyPressedLast && canExpandMist(player)) {
            val buf = PacketByteBuf(Unpooled.buffer())
            ClientPlayNetworking.send(id("expand_mist_form"), buf)
        }
        this.wasExpandKeyPressedLast = EXPAND_MIST_KEY.isPressed
    }

    fun canMist(player: PlayerEntity): Boolean {
        val abilityLevel: Int = player.getAbilityLevel(MIST_FORM)
        return abilityLevel > 0
    }

    fun canExpandMist(player: PlayerEntity): Boolean {
        return this.canMist(player) && MistFormAbilityComponent.entityKey.get(player).canExpandMist()
    }

    fun isInMistForm(player: PlayerEntity): Boolean {
        return MistFormAbilityComponent.entityKey.get(player).isInMistForm
    }
}