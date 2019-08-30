package com.williambl.haema.client.gui

import net.minecraft.client.Minecraft
import net.minecraft.client.gui.Gui
import net.minecraftforge.client.event.RenderGameOverlayEvent


class VampireOverlayGui : Gui() {

    fun renderOverlay(bloodLevel: Float, event: RenderGameOverlayEvent.Post) {
        drawCenteredString(
                Minecraft.getMinecraft().fontRenderer,
                "Blood Level: $bloodLevel",
                (event.resolution.scaledWidth * 2) / 3,
                event.resolution.scaledHeight / 2,
                0xffaa00
        )
    }

}