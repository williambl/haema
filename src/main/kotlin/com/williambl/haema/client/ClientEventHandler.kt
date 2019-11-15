package com.williambl.haema.client

import com.mojang.blaze3d.platform.GlStateManager
import com.williambl.haema.Haema
import com.williambl.haema.common.util.VampireAbilities
import com.williambl.haema.common.util.getVampirismCapability
import net.alexwells.kottle.KotlinEventBusSubscriber
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.AbstractGui.GUI_ICONS_LOCATION
import net.minecraft.client.gui.IngameGui
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.ForgeIngameGui
import net.minecraftforge.client.event.RenderGameOverlayEvent
import net.minecraftforge.event.TickEvent
import net.minecraftforge.eventbus.api.SubscribeEvent
import kotlin.math.roundToInt


@KotlinEventBusSubscriber
object ClientEventHandler {

    var shaderApplied = false

    private val halfIcon = ResourceLocation(Haema.MODID, "textures/gui/half.png")
    private val fullIcon = ResourceLocation(Haema.MODID, "textures/gui/full.png")

    @SubscribeEvent
    fun renderBloodLevelOverlay(e: RenderGameOverlayEvent.Post) {
        if (e.type != RenderGameOverlayEvent.ElementType.FOOD)
            return

        val player = Minecraft.getInstance().player
        val cap = player.getVampirismCapability()
        val mc = Minecraft.getInstance()

        cap.ifPresent { capability ->
            if (!capability.isVampire())
                return@ifPresent

            val bloodLevel = (capability.getBloodLevel()*20).roundToInt();
            if (bloodLevel <= 0) {
                return@ifPresent
            }

            GlStateManager.enableBlend()
            GlStateManager.pushMatrix()

            val top = mc.mainWindow.scaledHeight - ForgeIngameGui.right_height
            var right = mc.mainWindow.scaledWidth / 2 + 82

            var i = 1
            while (i < 20) {
                if (i < bloodLevel) {
                    //Full
                    fullIcon(fullIcon, right, top, 9)
                } else if (i == bloodLevel+1) {
                    //Half
                    halfIcon(halfIcon, right, top)
                }
                right -= 8
                i += 2
            }
            ForgeIngameGui.right_height += 10

            GlStateManager.popMatrix()
            GlStateManager.disableBlend()
            mc.getTextureManager().bindTexture(GUI_ICONS_LOCATION)
        }
    }

    private fun fullIcon(icon: ResourceLocation, right: Int, top: Int, width: Int): ResourceLocation {
        Minecraft.getInstance().getTextureManager().bindTexture(icon)
        GlStateManager.color4f(1f, 1f, 1f, 1f)
        IngameGui.blit(right, top, 0f, 0f, width, 9, 9, 9)
        return icon
    }

    private fun halfIcon(icon: ResourceLocation, right: Int, top: Int): ResourceLocation {
        Minecraft.getInstance().getTextureManager().bindTexture(icon)
        GlStateManager.color4f(1f, 1f, 1f, 1f)
        IngameGui.blit(right + 4, top, 0f, 0f, 5, 9, 5, 9)
        return icon
    }

    @SubscribeEvent
    fun applyShaderIfNeeded(e: TickEvent.ClientTickEvent) {
        val mc = Minecraft.getInstance()
        if (mc.player == null)
            return

        mc.player.getVampirismCapability().ifPresent {
            if (it.getAbilities() and VampireAbilities.VISION.flag != 0) {
                if (!shaderApplied || !mc.gameRenderer.isShaderActive) {
                    mc.gameRenderer.stopUseShader()
                    mc.gameRenderer.loadShader(enhancedVisionShaderLocation)
                    shaderApplied = true
                }
            } else if (shaderApplied) {
                mc.gameRenderer.stopUseShader()
            }
        }
    }
}