package com.williambl.haema.client

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.client.gui.VampireHud
import ladysnake.satin.api.event.ShaderEffectRenderCallback
import ladysnake.satin.api.managed.ManagedShaderEffect
import ladysnake.satin.api.managed.ShaderEffectManager
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper
import net.fabricmc.fabric.api.client.rendereregistry.v1.EntityRendererRegistry
import net.fabricmc.fabric.api.client.rendering.v1.ColorProviderRegistry
import net.fabricmc.fabric.api.client.rendering.v1.HudRenderCallback
import net.fabricmc.fabric.api.network.ClientSidePacketRegistry
import net.minecraft.client.MinecraftClient
import net.minecraft.client.color.item.ItemColorProvider
import net.minecraft.client.options.KeyBinding
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry
import org.lwjgl.glfw.GLFW


val VAMPIRE_SHADER: ManagedShaderEffect = ShaderEffectManager.getInstance()
    .manage(Identifier("haema", "shaders/post/vampirevision.json"))

val DASH_KEY = KeyBinding("key.haema.dash", GLFW.GLFW_KEY_X, "key.categories.movement")

var dashCooldownValue = 10

var distortAmount = 0.0f
    set(value) {
        field = value
        VAMPIRE_SHADER.setUniformValue("DistortAmount", value)
    }

fun setRedAmount(value: Float) {
    VAMPIRE_SHADER.setUniformValue("RedMatrix", value, 0.0f, 0.0f)
}

fun setColorScale(value: Float) {
    VAMPIRE_SHADER.setUniformValue("ColorScale", value, value, value)
}

fun setSaturation(value: Float) {
    VAMPIRE_SHADER.setUniformValue("Saturation", value)
}

fun init() {
    ClientSidePacketRegistry.INSTANCE.register(Identifier("haema:bloodlevelsync")) { packetContext, packetByteBuf ->
        if (packetContext.player is Vampirable) {
            (packetContext.player as Vampirable).checkBloodManager()
            (packetContext.player.hungerManager as VampireBloodManager).absoluteBloodLevel = packetByteBuf.readDouble()
        }
    }
    ClientSidePacketRegistry.INSTANCE.register(Identifier("haema:updatedashcooldown")) { packetContext, packetByteBuf ->
        dashCooldownValue = packetByteBuf.readInt()
    }

    HudRenderCallback.EVENT.register(VampireHud::render)

    ShaderEffectRenderCallback.EVENT.register(ShaderEffectRenderCallback {
        if ((MinecraftClient.getInstance().player as Vampirable).isVampire)
            VAMPIRE_SHADER.render(it)
    })

    KeyBindingHelper.registerKeyBinding(DASH_KEY)

    ColorProviderRegistry.ITEM.register(ItemColorProvider { stack, index ->
        if (index > 0) -1 else 0xA23C3A
    }, Registry.ITEM.get(Identifier("haema:vampire_blood")))

    EntityRendererRegistry.INSTANCE.register(Registry.ENTITY_TYPE.get(Identifier("haema:vampire_hunter"))) { dispatcher, _ -> VampireHunterEntityRenderer(dispatcher) }
}