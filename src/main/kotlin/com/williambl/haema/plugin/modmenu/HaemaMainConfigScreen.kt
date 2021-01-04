package com.williambl.haema.plugin.modmenu

import net.minecraft.client.MinecraftClient
import net.minecraft.client.gui.DrawableHelper
import net.minecraft.client.gui.screen.Screen
import net.minecraft.client.gui.widget.ButtonWidget
import net.minecraft.client.gui.widget.ButtonWidget.PressAction
import net.minecraft.client.util.InputUtil
import net.minecraft.client.util.math.MatrixStack
import net.minecraft.text.*
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier
import net.minecraft.util.Util
import org.lwjgl.glfw.GLFW
import java.awt.Color
import java.net.URI
import java.net.URISyntaxException

class HaemaMainConfigScreen(private val parent: Screen?) : Screen(LiteralText("HAEMA").formatted(Formatting.UNDERLINE)) {
    val icon = Identifier("haema:icon.png")

    override fun init() {
        super.init()
        addButton(ButtonWidget(width/4-75, 120, 150, 20, TranslatableText("gui.haema.config.client"), PressAction {
            onClose()
        }))
        addButton(ButtonWidget(3*width/4-75, 120, 150, 20, TranslatableText("gui.haema.config.gameplay"), PressAction {
            client?.openScreen(HaemaGameplayConfigScreen(this))
        }))
        addButton(ButtonWidget(width/2+10, 180, 200, 20, TranslatableText("gui.done"), PressAction {
            onClose()
        }))
    }

    override fun render(matrices: MatrixStack?, mouseX: Int, mouseY: Int, delta: Float) {
        renderBackground(matrices)
        super.render(matrices, mouseX, mouseY, delta)
        client!!.textureManager.bindTexture(icon)
        DrawableHelper.drawTexture(
            matrices,
            width/2-20,
            20,
            0f,
            0f,
            40,
            40,
            40,
            40
        )
    }

    override fun onClose() {
        client?.openScreen(parent)
    }
}