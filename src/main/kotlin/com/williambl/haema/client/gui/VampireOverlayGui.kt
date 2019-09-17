package com.williambl.haema.client.gui

import net.minecraft.client.Minecraft
import net.minecraft.client.gui.screen.Screen
import net.minecraft.util.text.StringTextComponent
import net.minecraftforge.client.event.RenderGameOverlayEvent


class VampireOverlayScreen : Screen(StringTextComponent("vampire_overlay")) {

    fun renderOverlay(bloodLevel: Float, event: RenderGameOverlayEvent.Post) {
        drawCenteredString(
                Minecraft.getInstance().fontRenderer,
                "Blood Level: $bloodLevel",
                (event.window.width * 2) / 3,
                event.window.height / 2,
                0xffaa00
        )
    }

}