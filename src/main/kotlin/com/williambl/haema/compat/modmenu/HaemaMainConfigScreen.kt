package com.williambl.haema.compat.modmenu

import com.williambl.haema.client.config.HaemaConfig
import com.williambl.haema.id
import com.williambl.haema.util.drawCenteredText
import me.shedaniel.autoconfig.AutoConfig
import net.minecraft.client.gui.DrawContext
import net.minecraft.client.gui.screen.Screen
import net.minecraft.client.gui.widget.ButtonWidget
import net.minecraft.text.Text
import net.minecraft.util.Formatting


class HaemaMainConfigScreen(private val parent: Screen?) : Screen(Text.literal("HAEMA").formatted(Formatting.UNDERLINE)) {
    val icon = id("icon.png")

    override fun init() {
        super.init()
        addDrawableChild(ButtonWidget.builder(Text.translatable("gui.haema.config.client")) {
            client?.setScreen(AutoConfig.getConfigScreen(HaemaConfig::class.java, this).get())
        }.dimensions(width / 4 - 75, 120, 150, 20).build())
        addDrawableChild(ButtonWidget.builder(Text.translatable("gui.haema.config.gameplay")) {
            client?.setScreen(HaemaGameplayConfigScreen(this))
        }.dimensions(3 * width / 4 - 75, 120, 150, 20).build())
        addDrawableChild(ButtonWidget.builder(Text.translatable("gui.done")) {
            close()
        }.dimensions(width / 2 - 100, 180, 200, 20).build())
    }

    override fun render(context: DrawContext, mouseX: Int, mouseY: Int, delta: Float) {
        renderBackground(context)
        super.render(context, mouseX, mouseY, delta)
        context.drawTexture(
                icon,
                width / 2 - 20,
                20,
                0f,
                0f,
                40,
                40,
                40,
                40
        )
        context.drawCenteredText(textRenderer, Text.translatable("gui.haema.title").formatted(Formatting.UNDERLINE), width / 2, 75, 0xffffff)
    }

    override fun close() {
        client?.setScreen(parent)
    }
}