package com.williambl.haema.client.gui

import com.williambl.haema.api.client.VampireHudAddTextEvent
import com.williambl.haema.client.HaemaClient
import com.williambl.haema.client.config.HudPlacement
import com.williambl.haema.isVampire
import net.minecraft.client.MinecraftClient
import net.minecraft.client.gui.DrawContext
import net.minecraft.text.MutableText
import net.minecraft.text.Text
import net.minecraft.util.Formatting

object VampireHud {
    fun render(context: DrawContext, tickDelta: Float) {
        if (HaemaClient.config.vampireHudPlacement == HudPlacement.NONE) return

        context.matrices.push()

        val mc = MinecraftClient.getInstance()
        val width = mc.window.scaledWidth
        val height = mc.window.scaledHeight
        val textRenderer = mc.textRenderer
        val player = mc.player ?: return

        if (!(player).isVampire)
            return

        val texts = VampireHudAddTextEvent.EVENT.invoker().addText(player, ::createText)

        texts.forEachIndexed { index, text ->
            context.drawCenteredTextWithShadow(
                    MinecraftClient.getInstance().textRenderer,
                    text,
                    HaemaClient.config.vampireHudPlacement.x(width, textRenderer.getWidth(text)),
                    HaemaClient.config.vampireHudPlacement.y(height, index),
                    0xffffff
            )
        }
        context.matrices.pop()
    }

    private fun createText(key: MutableText, available: Boolean, description: Text): Text {
        val keyText = key.formatted(if (available) Formatting.GREEN else Formatting.RED)
        return Text.literal("[").append(keyText).append("] ").append(description)
    }
}