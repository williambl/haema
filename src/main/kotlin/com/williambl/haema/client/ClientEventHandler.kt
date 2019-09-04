package com.williambl.haema.client

import com.williambl.haema.client.gui.VampireOverlayGui
import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.util.VampireAbilities
import com.williambl.haema.common.util.getVampirismCapability
import net.minecraft.client.Minecraft
import net.minecraftforge.client.event.RenderGameOverlayEvent
import net.minecraftforge.client.event.RenderWorldLastEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent

@Mod.EventBusSubscriber
object ClientEventHandler {

    val overlayGui: VampireOverlayGui = VampireOverlayGui()

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
    fun switchShader(e: RenderWorldLastEvent) {
        if (Minecraft.getMinecraft().player == null)
            return
        if (Minecraft.getMinecraft().player.getVampirismCapability().getAbilities() and VampireAbilities.VISION.flag != 0) {
            loadEnhancedVisionShader()
        } else {
            unloadShader()
        }
    }
}