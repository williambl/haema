package com.williambl.haema.client

import com.mojang.blaze3d.systems.RenderSystem
import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import ladysnake.satin.api.event.ShaderEffectRenderCallback
import ladysnake.satin.api.managed.ManagedShaderEffect
import ladysnake.satin.api.managed.ShaderEffectManager
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper
import net.fabricmc.fabric.api.client.rendering.v1.ColorProviderRegistry
import net.fabricmc.fabric.api.network.ClientSidePacketRegistry
import net.minecraft.client.MinecraftClient
import net.minecraft.client.color.item.ItemColorProvider
import net.minecraft.client.options.KeyBinding
import net.minecraft.util.Identifier
import net.minecraft.util.math.Vec3d
import net.minecraft.util.registry.Registry
import org.lwjgl.glfw.GLFW


val VAMPIRE_SHADER: ManagedShaderEffect = ShaderEffectManager.getInstance()
        .manage(Identifier("haema", "shaders/post/vampirevision.json"))

val DASH_KEY = KeyBinding("haema.key.dash", GLFW.GLFW_KEY_U, "key.categories.movement")

fun init() {
    ClientSidePacketRegistry.INSTANCE.register(Identifier("haema:bloodlevelsync")) { packetContext, packetByteBuf ->
        if (packetContext.player is Vampirable) {
            (packetContext.player as Vampirable).checkBloodManager()
            (packetContext.player.hungerManager as VampireBloodManager).absoluteBloodLevel = packetByteBuf.readDouble()
        }
    }
    ShaderEffectRenderCallback.EVENT.register(ShaderEffectRenderCallback {
        if ((MinecraftClient.getInstance().player as Vampirable).isVampire)
            VAMPIRE_SHADER.render(it)
        RenderSystem.enableTexture()
    })

    KeyBindingHelper.registerKeyBinding(DASH_KEY)

    ColorProviderRegistry.ITEM.register(ItemColorProvider { stack, index ->
        if (index > 0) -1 else 0xA23C3A
    }, Registry.ITEM.get(Identifier("haema:vampire_blood")))
}