package com.williambl.haema.client

import com.mojang.blaze3d.systems.RenderSystem
import com.williambl.haema.Vampirable
import com.williambl.haema.VampireAbility
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.client.config.HaemaConfig
import com.williambl.haema.client.gui.RitualTableScreen
import com.williambl.haema.client.gui.VampireHud
import com.williambl.haema.ritual.RitualTableScreenHandler
import ladysnake.satin.api.event.ShaderEffectRenderCallback
import ladysnake.satin.api.managed.ManagedShaderEffect
import ladysnake.satin.api.managed.ShaderEffectManager
import me.sargunvohra.mcmods.autoconfig1u.AutoConfig
import me.sargunvohra.mcmods.autoconfig1u.serializer.Toml4jConfigSerializer
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper
import net.fabricmc.fabric.api.client.rendereregistry.v1.EntityRendererRegistry
import net.fabricmc.fabric.api.client.rendering.v1.HudRenderCallback
import net.fabricmc.fabric.api.client.screenhandler.v1.ScreenRegistry
import net.fabricmc.fabric.api.network.ClientSidePacketRegistry
import net.minecraft.client.MinecraftClient
import net.minecraft.client.options.KeyBinding
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.text.Text
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry
import org.lwjgl.glfw.GLFW


val VAMPIRE_SHADER: ManagedShaderEffect = ShaderEffectManager.getInstance()
    .manage(Identifier("haema", "shaders/post/vampirevision.json"))

val DASH_KEY = KeyBinding("key.haema.dash", GLFW.GLFW_KEY_Z, "key.categories.movement")

val config: HaemaConfig by lazy { AutoConfig.getConfigHolder(HaemaConfig::class.java).config }

var dashCooldownValue = 10

var distortAmount = 0.0f
    set(value) {
        field = value * config.distortionAdjust * MinecraftClient.getInstance().options.distortionEffectScale
        VAMPIRE_SHADER.setUniformValue("DistortAmount", field)
    }

fun setRedAmount(value: Float) {
    VAMPIRE_SHADER.setUniformValue("RedMatrix", value, 0.0f, 0.0f)
}

fun setColorScale(value: Float) {
    VAMPIRE_SHADER.setUniformValue("ColorScale", value*config.brightAdjust, value*config.brightAdjust, value* config.brightAdjust)
}

fun setSaturation(value: Float) {
    VAMPIRE_SHADER.setUniformValue("Saturation", 1f-((1f-value)*config.saturationAdjust))
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
        if (config.vampireShaderEnabled && (MinecraftClient.getInstance().player as Vampirable).isVampire && (MinecraftClient.getInstance().player as Vampirable).getAbilityLevel(VampireAbility.VISION) > 0) {
            RenderSystem.disableAlphaTest();
            VAMPIRE_SHADER.render(it)
        }
    })

    KeyBindingHelper.registerKeyBinding(DASH_KEY)

    //ColorProviderRegistry.ITEM.register(ItemColorProvider { stack, index ->
    //    if (index > 0) -1 else 0xA23C3A
    //}, Registry.ITEM.get(Identifier("haema:vampire_blood")))

    EntityRendererRegistry.INSTANCE.register(Registry.ENTITY_TYPE.get(Identifier("haema:vampire_hunter"))) { dispatcher, _ -> VampireHunterEntityRenderer(dispatcher) }

    AutoConfig.register(HaemaConfig::class.java) { config, clazz -> Toml4jConfigSerializer(config, clazz) }

    ScreenRegistry.register(RitualTableScreenHandler.ritualTableScreenHandlerType) {
            screenHandler: RitualTableScreenHandler, inv: PlayerInventory, title: Text -> RitualTableScreen(screenHandler, inv, title)
    }
}