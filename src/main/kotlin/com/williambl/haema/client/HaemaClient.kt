package com.williambl.haema.client

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.component.invisibility.InvisibilityAbilityComponent
import com.williambl.haema.api.client.VampireHudAddTextEvent
import com.williambl.haema.client.config.HaemaConfig
import com.williambl.haema.client.gui.RitualTableScreen
import com.williambl.haema.client.gui.VampireHud
import com.williambl.haema.component.EntityVampireComponent
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.hunter.VampireHunterModule
import com.williambl.haema.id
import com.williambl.haema.isVampire
import com.williambl.haema.ritual.RitualTableScreenHandler
import com.williambl.haema.vampireComponent
import ladysnake.satin.api.event.ShaderEffectRenderCallback
import ladysnake.satin.api.managed.ManagedShaderEffect
import ladysnake.satin.api.managed.ShaderEffectManager
import me.shedaniel.autoconfig.AutoConfig
import me.shedaniel.autoconfig.serializer.Toml4jConfigSerializer
import net.fabricmc.api.ClientModInitializer
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking
import net.fabricmc.fabric.api.client.rendering.v1.EntityModelLayerRegistry
import net.fabricmc.fabric.api.client.rendering.v1.EntityRendererRegistry
import net.fabricmc.fabric.api.client.rendering.v1.HudRenderCallback
import net.fabricmc.fabric.api.client.screenhandler.v1.ScreenRegistry
import net.minecraft.client.MinecraftClient
import net.minecraft.client.option.KeyBinding
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.util.hit.EntityHitResult
import net.minecraft.util.hit.HitResult
import org.lwjgl.glfw.GLFW

object HaemaClient: ClientModInitializer {
    val VAMPIRE_SHADER: ManagedShaderEffect = ShaderEffectManager.getInstance()
        .manage(id("shaders/post/vampirevision.json"))

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

    override fun onInitializeClient() {
        ClientPlayNetworking.registerGlobalReceiver(id("updatedashcooldown")) { client, handler, buf, sender ->
            dashCooldownValue = buf.readInt()
        }
        ClientPlayNetworking.registerGlobalReceiver(id("updateinvislength")) { client, handler, buf, sender ->
            invisLengthValue = buf.readInt()
        }

        HudRenderCallback.EVENT.register(VampireHud::render)

        ShaderEffectRenderCallback.EVENT.register(ShaderEffectRenderCallback {
            if (config.vampireShaderEnabled && (MinecraftClient.getInstance().player)!!.isVampire == true && (MinecraftClient.getInstance().player)!!.getAbilityLevel(
                    AbilityModule.VISION) > 0) {
                VAMPIRE_SHADER.render(it)
            }
        })

        KeyBindingHelper.registerKeyBinding(DASH_KEY)

        EntityModelLayerRegistry.registerModelLayer(VampireHunterModel.layer, VampireHunterModel.Companion::getTexturedModelData)
        EntityRendererRegistry.register(VampireHunterModule.VAMPIRE_HUNTER) { context -> VampireHunterEntityRenderer(context) }

        AutoConfig.register(HaemaConfig::class.java) { config, clazz -> Toml4jConfigSerializer(config, clazz) }

        ScreenRegistry.register(RitualTableScreenHandler.ritualTableScreenHandlerType) {
                screenHandler: RitualTableScreenHandler, inv: PlayerInventory, title: Text -> RitualTableScreen(screenHandler, inv, title)
        }

        VampireHudAddTextEvent.EVENT.register(VampireHudAddTextEvent { player, createText ->
            val dashLevel = (player).getAbilityLevel(AbilityModule.DASH)
            if (dashLevel > 0 && (player.vampireComponent).blood > 18f) {
                return@VampireHudAddTextEvent listOf(createText(
                    DASH_KEY.boundKeyLocalizedText.copy(),
                    (player).isVampire && (player as ClientVampire).canDash(),
                    TranslatableText("gui.haema.hud.vampiredash")
                ))
            }
            return@VampireHudAddTextEvent emptyList()
        })

        VampireHudAddTextEvent.EVENT.register(VampireHudAddTextEvent { player, createText ->
            val invisLevel = (player).getAbilityLevel(AbilityModule.INVISIBILITY)
            if (invisLevel > 0 && (player.vampireComponent).blood >= 18f) {
                return@VampireHudAddTextEvent listOf(
                    createText(
                        MinecraftClient.getInstance().options.keySneak.boundKeyLocalizedText.copy(),
                        (player).isVampire && player.world.time - InvisibilityAbilityComponent.entityKey.get(player).invisTicks >= 120 + invisLevel * invisLengthValue,
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
                if (EntityVampireComponent.poorBloodTag.contains(lookingAt) || EntityVampireComponent.mediumBloodTag.contains(lookingAt) || EntityVampireComponent.goodBloodTag.contains(lookingAt)) {
                    if (player.isSneaking) {
                        texts.add(
                            TranslatableText("gui.haema.hud.bloodquality").append(
                                when {
                                    EntityVampireComponent.goodBloodTag.contains(lookingAt) -> TranslatableText("gui.haema.hud.bloodquality.good").formatted(
                                        Formatting.GREEN
                                    )
                                    EntityVampireComponent.mediumBloodTag.contains(lookingAt) -> TranslatableText("gui.haema.hud.bloodquality.medium").formatted(
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
}