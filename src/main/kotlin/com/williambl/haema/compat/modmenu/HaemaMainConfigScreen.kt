package com.williambl.haema.compat.modmenu

import com.williambl.haema.client.config.HaemaConfig
import me.sargunvohra.mcmods.autoconfig1u.AutoConfig
import net.minecraft.client.gui.DrawableHelper
import net.minecraft.client.gui.screen.Screen
import net.minecraft.client.gui.widget.ButtonWidget
import net.minecraft.client.gui.widget.ButtonWidget.PressAction
import net.minecraft.client.util.math.MatrixStack
import net.minecraft.text.LiteralText
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier


class HaemaMainConfigScreen(private val parent: Screen?) : Screen(LiteralText("HAEMA").formatted(Formatting.UNDERLINE)) {
    val icon = Identifier("haema:icon.png")

    override fun init() {
        super.init()
        addButton(ButtonWidget(width / 4 - 75, 120, 150, 20, TranslatableText("gui.haema.config.client"), PressAction {
            client?.openScreen(AutoConfig.getConfigScreen(HaemaConfig::class.java, this).get())
        }))
        addButton(ButtonWidget(3 * width / 4 - 75, 120, 150, 20, TranslatableText("gui.haema.config.gameplay"), PressAction {
            client?.openScreen(HaemaGameplayConfigScreen(this))
        }))
        addButton(ButtonWidget(width / 2 - 100, 180, 200, 20, TranslatableText("gui.done"), PressAction {
            onClose()
        }))
    }

    override fun render(matrices: MatrixStack?, mouseX: Int, mouseY: Int, delta: Float) {
        renderBackground(matrices)
        super.render(matrices, mouseX, mouseY, delta)
        client!!.textureManager.bindTexture(icon)
        DrawableHelper.drawTexture(
                matrices,
                width / 2 - 20,
                20,
                0f,
                0f,
                40,
                40,
                40,
                40
        )
        DrawableHelper.drawCenteredText(matrices, textRenderer, TranslatableText("gui.haema.title").formatted(Formatting.UNDERLINE), width / 2, 75, 0xffffff)
    }

    override fun onClose() {
        client?.openScreen(parent)
    }
}