package com.williambl.haema.client

import com.williambl.haema.client.gui.VampireOverlayGui
import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.util.VampireAbilities
import com.williambl.haema.common.util.getVampirismCapability
import net.minecraft.client.Minecraft
import net.minecraftforge.client.event.RenderGameOverlayEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.fml.common.gameevent.TickEvent

@Mod.EventBusSubscriber
object ClientEventHandler {

    val overlayGui: VampireOverlayGui = VampireOverlayGui()
    var shaderApplied = false

    @SubscribeEvent
    @JvmStatic
    fun renderBloodLevelOverlay(e: RenderGameOverlayEvent.Post) {
        if (e.type != RenderGameOverlayEvent.ElementType.ALL)
            return

        val player = Minecraft.getMinecraft().player
        val cap = player.getCapability(VampirismProvider.vampirism!!, null)!!

        if (!cap.isVampire())
            return

        overlayGui.renderOverlay(cap.getBloodLevel(), e)
    }

    @SubscribeEvent
    @JvmStatic
    fun applyShaderIfNeeded(e: TickEvent.ClientTickEvent) {
        val mc = Minecraft.getMinecraft()
        if (mc.player == null)
            return

        if (mc.player.getVampirismCapability().getAbilities() and VampireAbilities.VISION.flag != 0) {
            if (!shaderApplied || !mc.entityRenderer.isShaderActive) {
                mc.entityRenderer.stopUseShader()
                mc.entityRenderer.loadShader(enhancedVisionShaderLocation)
                shaderApplied = true
            }
        } else if (shaderApplied) {
            mc.entityRenderer.stopUseShader()
        }
    }
}