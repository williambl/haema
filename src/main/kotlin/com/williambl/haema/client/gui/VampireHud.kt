package com.williambl.haema.client.gui

import com.mojang.blaze3d.systems.RenderSystem
import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.client.ClientVampire
import com.williambl.haema.client.DASH_KEY
import com.williambl.haema.client.config
import com.williambl.haema.client.config.HudPlacement
import net.minecraft.client.MinecraftClient
import net.minecraft.client.gui.DrawableHelper
import net.minecraft.client.options.KeyBinding
import net.minecraft.client.util.math.MatrixStack
import net.minecraft.text.LiteralText
import net.minecraft.text.MutableText
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting

object VampireHud : DrawableHelper() {

    fun render(matrixStack: MatrixStack, tickDelta: Float) {
        if (config.vampireHudPlacement == HudPlacement.NONE) return

        matrixStack.push()
        val width = MinecraftClient.getInstance().window.scaledWidth
        val height = MinecraftClient.getInstance().window.scaledHeight
        val textRenderer = MinecraftClient.getInstance().textRenderer
        val player = MinecraftClient.getInstance().player
        (player as Vampirable).checkBloodManager()

        val texts = mutableListOf<Text>()

        val hasEnoughBlood = (player as Vampirable).isVampire && (player.hungerManager as VampireBloodManager).getBloodLevel() > 18f
        val canDash = (player as Vampirable).isVampire && (player as ClientVampire).canDash()

        if (hasEnoughBlood) {
            texts.add(createText(DASH_KEY.boundKeyLocalizedText.copy(), if (canDash) Formatting.GREEN else Formatting.RED, TranslatableText("gui.haema.hud.vampiredash")))
        }

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


    private fun createText(key: MutableText, formatting: Formatting, description: Text): Text {
        val keyText = key.formatted(formatting)
        val text = LiteralText("[")
        text.siblings.add(keyText)
        text.siblings.add(LiteralText("] "))
        text.siblings.add(description)
        return text
    }
}