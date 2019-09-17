package com.williambl.haema.client

import com.williambl.haema.client.gui.VampireOverlayScreen
import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.util.VampireAbilities
import com.williambl.haema.common.util.getVampirismCapability
import net.alexwells.kottle.KotlinEventBusSubscriber
import net.minecraft.client.Minecraft
import net.minecraftforge.client.event.RenderGameOverlayEvent
import net.minecraftforge.event.TickEvent
import net.minecraftforge.eventbus.api.SubscribeEvent

@KotlinEventBusSubscriber
object ClientEventHandler {

    val overlayGui: VampireOverlayScreen = VampireOverlayScreen()
    var shaderApplied = false

    @SubscribeEvent
    @JvmStatic
    fun renderBloodLevelOverlay(e: RenderGameOverlayEvent.Post) {
        if (e.type != RenderGameOverlayEvent.ElementType.ALL)
            return

        val player = Minecraft.getInstance().player
        val cap = player.getCapability(VampirismProvider.vampirism!!, null)!!

        if (!cap.isVampire())
            return

        overlayGui.renderOverlay(cap.getBloodLevel(), e)
    }

    @SubscribeEvent
    @JvmStatic
    fun applyShaderIfNeeded(e: TickEvent.ClientTickEvent) {
        val mc = Minecraft.getInstance()
        if (mc.player == null)
            return

        if (mc.player.getVampirismCapability().getAbilities() and VampireAbilities.VISION.flag != 0) {
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