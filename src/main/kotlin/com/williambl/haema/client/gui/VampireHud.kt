package com.williambl.haema.client.gui

import com.williambl.haema.Vampirable
import com.williambl.haema.api.client.VampireHudAddTextEvent
import com.williambl.haema.client.config
import com.williambl.haema.client.config.HudPlacement
import com.williambl.haema.client.invisLengthValue
import net.minecraft.client.MinecraftClient
import net.minecraft.client.gui.DrawableHelper
import net.minecraft.client.util.math.MatrixStack
import net.minecraft.text.LiteralText
import net.minecraft.text.MutableText
import net.minecraft.text.Text
import net.minecraft.util.Formatting

object VampireHud : DrawableHelper() {
    fun render(matrixStack: MatrixStack, @Suppress("UNUSED_PARAMETER") tickDelta: Float) {
        if (config.vampireHudPlacement == HudPlacement.NONE) return

        matrixStack.push()

        val mc = MinecraftClient.getInstance()
        val width = mc.window.scaledWidth
        val height = mc.window.scaledHeight
        val textRenderer = mc.textRenderer
        val player = mc.player

        if (!(player as Vampirable).isVampire)
            return

        (player as Vampirable).checkBloodManager()

        val texts = VampireHudAddTextEvent.EVENT.invoker().addText(player, ::createText)

        texts.forEachIndexed { index, text ->
            drawCenteredText(
                    matrixStack,
                    MinecraftClient.getInstance().textRenderer,
                    text,
                    config.vampireHudPlacement.x(width, textRenderer.getWidth(text)),
                    config.vampireHudPlacement.y(height,index),
                    0xffffff
            )
        }
        matrixStack.pop()
    }

    private fun createText(key: MutableText, available: Boolean, description: Text): Text {
        val keyText = key.formatted(if (available) Formatting.GREEN else Formatting.RED)
        return LiteralText("[").append(keyText).append("] ").append(description)
    }
}