package com.williambl.haema.client

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.api.client.VampireHudAddTextEvent
import com.williambl.haema.client.config.HaemaConfig
import com.williambl.haema.client.gui.RitualTableScreen
import com.williambl.haema.client.gui.VampireHud
import com.williambl.haema.hunter.VampireHunterEntity
import com.williambl.haema.ritual.RitualTableScreenHandler
import ladysnake.satin.api.event.ShaderEffectRenderCallback
import ladysnake.satin.api.managed.ManagedShaderEffect
import ladysnake.satin.api.managed.ShaderEffectManager
import me.shedaniel.autoconfig.AutoConfig
import me.shedaniel.autoconfig.serializer.Toml4jConfigSerializer
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking
import net.fabricmc.fabric.api.client.rendereregistry.v1.EntityModelLayerRegistry
import net.fabricmc.fabric.api.client.rendereregistry.v1.EntityRendererRegistry
import net.fabricmc.fabric.api.client.rendering.v1.HudRenderCallback
import net.fabricmc.fabric.api.client.screenhandler.v1.ScreenRegistry
import net.minecraft.client.MinecraftClient
import net.minecraft.client.option.KeyBinding
import net.minecraft.entity.EntityType
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier
import net.minecraft.util.hit.EntityHitResult
import net.minecraft.util.hit.HitResult
import net.minecraft.util.registry.Registry
import org.lwjgl.glfw.GLFW

val VAMPIRE_SHADER: ManagedShaderEffect = ShaderEffectManager.getInstance()
    .manage(Identifier("haema", "shaders/post/vampirevision.json"))

val DASH_KEY = KeyBinding("key.haema.dash", GLFW.GLFW_KEY_Z, "key.categories.movement")

val config: HaemaConfig by lazy { AutoConfig.getConfigHolder(HaemaConfig::class.java).config }

var dashCooldownValue = 10

var invisLengthValue = 10

var distortAmount = 0.0f
    set(value) {
        field = value * config.distortionAdjust * MinecraftClient.getInstance().options.distortionEffectScale
        VAMPIRE_SHADER.setUniformValue("DistortAmount", field)
    }

fun setRedAmount(value: Float) {
    VAMPIRE_SHADER.setUniformValue("RedMatrix", value, 0.0f, 0.0f)
}

fun setBrightnessAdjust(value: Float) {
    VAMPIRE_SHADER.setUniformValue("BrightnessAdjust", value*config.brightAdjust)
}

fun setSaturation(value: Float) {
    VAMPIRE_SHADER.setUniformValue("Saturation", 1f-((1f-value)*config.saturationAdjust))
}

fun init() {
    ClientPlayNetworking.registerGlobalReceiver(Identifier("haema:bloodlevelsync")) { client, handler, buf, sender ->
        if (client.player is Vampirable) {
            (client.player as Vampirable).checkBloodManager()
            (client.player?.hungerManager as VampireBloodManager).absoluteBloodLevel = buf.readDouble()
        }
    }
    ClientPlayNetworking.registerGlobalReceiver(Identifier("haema:updatedashcooldown")) { client, handler, buf, sender ->
        dashCooldownValue = buf.readInt()
    }
    ClientPlayNetworking.registerGlobalReceiver(Identifier("haema:updateinvislength")) { client, handler, buf, sender ->
        invisLengthValue = buf.readInt()
    }
    ClientPlayNetworking.registerGlobalReceiver(Identifier("haema:updateinvisticks")) { client, handler, buf, sender ->
        (client.player!!.hungerManager as VampireBloodManager).invisTicks = client.world!!.time
    }

    HudRenderCallback.EVENT.register(VampireHud::render)

    ShaderEffectRenderCallback.EVENT.register(ShaderEffectRenderCallback {
        if (config.vampireShaderEnabled && (MinecraftClient.getInstance().player as Vampirable).isVampire && (MinecraftClient.getInstance().player as Vampirable).getAbilityLevel(
                AbilityModule.VISION) > 0) {
            VAMPIRE_SHADER.render(it)
        }
    })

    KeyBindingHelper.registerKeyBinding(DASH_KEY)

    @Suppress("UnstableApiUsage", "DEPRECATION")
    EntityModelLayerRegistry.registerModelLayer(VampireHunterModel.layer, VampireHunterModel.Companion::getTexturedModelData)
    @Suppress("UNCHECKED_CAST")
    EntityRendererRegistry.INSTANCE.register(Registry.ENTITY_TYPE.get(Identifier("haema:vampire_hunter")) as EntityType<VampireHunterEntity>) { context -> VampireHunterEntityRenderer(context) }

    AutoConfig.register(HaemaConfig::class.java) { config, clazz -> Toml4jConfigSerializer(config, clazz) }

    ScreenRegistry.register(RitualTableScreenHandler.ritualTableScreenHandlerType) {
            screenHandler: RitualTableScreenHandler, inv: PlayerInventory, title: Text -> RitualTableScreen(screenHandler, inv, title)
    }

    VampireHudAddTextEvent.EVENT.register(VampireHudAddTextEvent { player, createText ->
        val dashLevel = (player as Vampirable).getAbilityLevel(AbilityModule.DASH)
        if (dashLevel > 0 && (player.hungerManager as VampireBloodManager).getBloodLevel() > 18f) {
            return@VampireHudAddTextEvent listOf(createText(
                DASH_KEY.boundKeyLocalizedText.copy(),
                (player as Vampirable).isVampire && (player as ClientVampire).canDash(),
                TranslatableText("gui.haema.hud.vampiredash")
            ))
        }
        return@VampireHudAddTextEvent emptyList()
    })

    VampireHudAddTextEvent.EVENT.register(VampireHudAddTextEvent { player, createText ->
        val invisLevel = (player as Vampirable).getAbilityLevel(AbilityModule.INVISIBILITY)
        if (invisLevel > 0 && (player.hungerManager as VampireBloodManager).getBloodLevel() >= 18f) {
            return@VampireHudAddTextEvent listOf(
                createText(
                    MinecraftClient.getInstance().options.keySneak.boundKeyLocalizedText.copy(),
                    (player as Vampirable).isVampire && player.world.time - (player.hungerManager as VampireBloodManager).invisTicks >= 120 + invisLevel * invisLengthValue,
                    TranslatableText("gui.haema.hud.invisibility")
                )
            )
        }
        return@VampireHudAddTextEvent emptyList()
    })

    VampireHudAddTextEvent.EVENT.register(VampireHudAddTextEvent { player, createText ->
        val texts = mutableListOf<Text>()
        val mc = MinecraftClient.getInstance()
        if (mc.crosshairTarget != null && mc.crosshairTarget!!.type == HitResult.Type.ENTITY) {
            val lookingAt = (mc.crosshairTarget as EntityHitResult).entity.type
            if (VampireBloodManager.poorBloodTag.contains(lookingAt) || VampireBloodManager.mediumBloodTag.contains(lookingAt) || VampireBloodManager.goodBloodTag.contains(lookingAt)) {
                if (player.isSneaking) {
                    texts.add(
                        TranslatableText("gui.haema.hud.bloodquality").append(
                            when {
                                VampireBloodManager.goodBloodTag.contains(lookingAt) -> TranslatableText("gui.haema.hud.bloodquality.good").formatted(
                                    Formatting.GREEN
                                )
                                VampireBloodManager.mediumBloodTag.contains(lookingAt) -> TranslatableText("gui.haema.hud.bloodquality.medium").formatted(
                                    Formatting.YELLOW
                                )
                                else -> TranslatableText("gui.haema.hud.bloodquality.poor").formatted(Formatting.RED)
                            }
                        )
                    )
                }

                texts.add(
                    createText(
                        TranslatableText("key.sneak").append(" + ").append(mc.options.keyUse.boundKeyLocalizedText),
                        true,
                        TranslatableText("gui.haema.hud.feed")
                    )
                )
            }
        }
        return@VampireHudAddTextEvent texts
    })
}