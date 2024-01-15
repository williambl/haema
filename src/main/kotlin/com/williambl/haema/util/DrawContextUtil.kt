package com.williambl.haema.util

import net.minecraft.client.font.TextRenderer
import net.minecraft.client.gui.DrawContext
import net.minecraft.text.OrderedText
import net.minecraft.text.Text

fun DrawContext.drawCenteredText(textRenderer: TextRenderer, text: String?, centerX: Int, y: Int, color: Int) {
    this.drawText(textRenderer, text, centerX - textRenderer.getWidth(text) / 2, y, color, false)
}

fun DrawContext.drawCenteredText(textRenderer: TextRenderer, text: Text, centerX: Int, y: Int, color: Int) {
    val orderedText = text.asOrderedText()
    this.drawText(textRenderer, orderedText, centerX - textRenderer.getWidth(orderedText) / 2, y, color, false)
}

fun DrawContext.drawCenteredText(textRenderer: TextRenderer, text: OrderedText?, centerX: Int, y: Int, color: Int) {
    this.drawText(textRenderer, text, centerX - textRenderer.getWidth(text) / 2, y, color, false)
}
