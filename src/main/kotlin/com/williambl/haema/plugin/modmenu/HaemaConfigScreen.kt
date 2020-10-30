package com.williambl.haema.plugin.modmenu

import net.minecraft.client.MinecraftClient
import net.minecraft.client.gui.DrawableHelper
import net.minecraft.client.gui.screen.Screen
import net.minecraft.client.util.InputUtil
import net.minecraft.client.util.math.MatrixStack
import net.minecraft.text.ClickEvent
import net.minecraft.text.LiteralText
import net.minecraft.text.Style
import net.minecraft.util.Formatting
import net.minecraft.util.Util
import org.lwjgl.glfw.GLFW
import java.net.URI
import java.net.URISyntaxException

class HaemaConfigScreen(private val parent: Screen?) : Screen(LiteralText("HAEMA").formatted(Formatting.UNDERLINE)) {

    val keys = listOf(
        GLFW.GLFW_KEY_UP,
        GLFW.GLFW_KEY_UP,
        GLFW.GLFW_KEY_DOWN,
        GLFW.GLFW_KEY_DOWN,
        GLFW.GLFW_KEY_LEFT,
        GLFW.GLFW_KEY_RIGHT,
        GLFW.GLFW_KEY_LEFT,
        GLFW.GLFW_KEY_RIGHT,
        GLFW.GLFW_KEY_B,
        GLFW.GLFW_KEY_A
    )

    var wasKeyDown = false
    var currentKey = 0

    val texts = listOf(
        LiteralText("Haema is configured using data packs and game rules.").formatted(Formatting.UNDERLINE),
        LiteralText("To learn how to use game rules, click here.")
            .setStyle(Style.EMPTY
                .withClickEvent(
                    ClickEvent(ClickEvent.Action.OPEN_URL, "https://minecraft.gamepedia.com/Commands/gamerule")
                )
                .withUnderline(true)
                .withColor(Formatting.BLUE)
            ),
        LiteralText("To learn how to use data packs, click here.")
            .setStyle(Style.EMPTY
                .withClickEvent(
                    ClickEvent(ClickEvent.Action.OPEN_URL, "https://minecraft.gamepedia.com/Data_Pack")
                )
                .withUnderline(true)
                .withColor(Formatting.BLUE)
            )
    )

    override fun render(matrices: MatrixStack?, mouseX: Int, mouseY: Int, delta: Float) {
        renderBackground(matrices)
        DrawableHelper.drawTexture(
            matrices,
            width/20,
            20,
            0f,
            0f,
            20,
            20,
            9,
            9
        )
        DrawableHelper.drawCenteredText(
            matrices,
            textRenderer,
            texts[0],
            width / 2,
            40,
            0xffffff
        )
        DrawableHelper.drawCenteredText(
            matrices,
            textRenderer,
            texts[1],
            width / 2,
            80,
            0xffffff
        )
        DrawableHelper.drawCenteredText(
            matrices,
            textRenderer,
            texts[2],
            width / 2,
            90,
            0xffffff
        )
    }

    override fun onClose() {
        client?.openScreen(parent)
    }

    override fun tick() {
        super.tick()
        if (currentKey > 9) {
            currentKey = 0
            println("well done")
        }
        if (wasKeyDown) {
            wasKeyDown = false
        } else {
            if (InputUtil.isKeyPressed(MinecraftClient.getInstance().window.handle, keys[currentKey])) {
                currentKey++
                wasKeyDown = true
            }
        }
    }

    override fun mouseClicked(mouseX: Double, mouseY: Double, button: Int): Boolean {
        val style: Style? = getTextComponentUnderMouse(mouseX.toInt(), mouseY.toInt())
        if (style?.clickEvent != null && style.clickEvent!!.action == ClickEvent.Action.OPEN_URL) {
            handleTextClick(style)
            return false
        }
        return super.mouseClicked(mouseX, mouseY, button)
    }

    override fun handleTextClick(style: Style?): Boolean {
        val clickEvent = style?.clickEvent
        if (clickEvent == null) {
            return false
        } else if (clickEvent.action == ClickEvent.Action.OPEN_URL) {
            if (clickEvent.action == ClickEvent.Action.OPEN_URL) {
                return try {
                    Util.getOperatingSystem().open(URI(clickEvent.value))
                    true
                } catch (e: URISyntaxException) {
                    false
                }
            }
        }
        return false
    }

    private fun getTextComponentUnderMouse(mouseX: Int, mouseY: Int): Style? {
        val textIndex = when (mouseY) {
            in 40..49 -> 0
            in 80..89 -> 1
            in 90..99 -> 2
            else -> 0
        }
        val i = client!!.textRenderer.getWidth(texts[textIndex])
        val j = width / 2 - i / 2
        val k = width / 2 + i / 2
        if (mouseX in j..k) {
            return client!!.textRenderer.textHandler.getStyleAt(texts[textIndex], mouseX - j)
        }
        return null
    }
}