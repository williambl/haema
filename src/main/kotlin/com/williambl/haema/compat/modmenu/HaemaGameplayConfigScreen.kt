package com.williambl.haema.compat.modmenu

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

class HaemaGameplayConfigScreen(private val parent: Screen?) : Screen(LiteralText("HAEMA").formatted(Formatting.UNDERLINE)) {

    val icon = Identifier("haema:icon.png")

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
    var currentHue = 0.0
    var isDoingAnim = false

    var isShowingMore = false

    private val texts = listOf(
        TranslatableText("gui.haema.config.gameplay.main").formatted(Formatting.UNDERLINE),
        TranslatableText("gui.haema.config.gameplay.gameruleslink")
            .setStyle(Style.EMPTY
                .withClickEvent(
                    ClickEvent(ClickEvent.Action.OPEN_URL, "https://minecraft.gamepedia.com/Commands/gamerule")
                )
                .withUnderline(true)
                .withColor(Formatting.BLUE)
            ),
        TranslatableText("gui.haema.config.gameplay.datapackslink")
            .setStyle(Style.EMPTY
                .withClickEvent(
                    ClickEvent(ClickEvent.Action.OPEN_URL, "https://minecraft.gamepedia.com/Data_Pack")
                )
                .withUnderline(true)
                .withColor(Formatting.BLUE)
            )
    )
    private val extratexts = listOf(
        TranslatableText("gui.haema.config.gameplay.bloodsources"),
        LiteralText("haema:good_blood_sources").formatted(Formatting.UNDERLINE),
        LiteralText("haema:medium_blood_sources").formatted(Formatting.UNDERLINE),
        LiteralText("haema:poor_blood_sources").formatted(Formatting.UNDERLINE),

        TranslatableText("gui.haema.config.gameplay.vampireweapons")
            .append(LiteralText("haema:vampire_weapons").formatted(Formatting.UNDERLINE)),

        TranslatableText("gui.haema.config.gameplay.gamerules")
    )

    override fun init() {
        super.init()
        addButton(object : ButtonWidget(width/2-210, 180, 200, 20, TranslatableText("gui.haema.moreinfo"), PressAction {
            isShowingMore = !isShowingMore
        }) {
            override fun getMessage(): Text {
                return TranslatableText(if (isShowingMore) "gui.haema.lessinfo" else "gui.haema.moreinfo")
            }
        })
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
        if (isShowingMore) {
            DrawableHelper.drawCenteredText(matrices, textRenderer, extratexts[0], width / 2, 80, 0xffffff)
            DrawableHelper.drawCenteredText(matrices, textRenderer, extratexts[1], width / 2, 90, 0xffffff)
            DrawableHelper.drawCenteredText(matrices, textRenderer, extratexts[2], width / 2, 100, 0xffffff)
            DrawableHelper.drawCenteredText(matrices, textRenderer, extratexts[3], width / 2, 110, 0xffffff)
            DrawableHelper.drawCenteredText(matrices, textRenderer, extratexts[4], width / 2, 130, 0xffffff)
            DrawableHelper.drawCenteredText(matrices, textRenderer, extratexts[5], width / 2, 150, 0xffffff)
        } else {
            DrawableHelper.drawCenteredText(matrices, textRenderer, texts[0], width / 2, 80, if (isDoingAnim) Color.HSBtoRGB(currentHue.toFloat(), 0.8f, 0.8f) else 0xffffff)
            DrawableHelper.drawCenteredText(matrices, textRenderer, texts[1], width / 2, 120, 0xffffff)
            DrawableHelper.drawCenteredText(matrices, textRenderer, texts[2], width / 2, 135, 0xffffff)
        }
    }

    override fun onClose() {
        client?.openScreen(parent)
    }

    override fun tick() {
        super.tick()
        if (isDoingAnim && currentHue < 1) {
            currentHue += 0.01
        } else if (currentHue >= 1) {
            currentHue = 0.0
            isDoingAnim = false
        }
        if (currentKey > 9) {
            currentKey = 0
            isDoingAnim = true
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
        if (!isShowingMore) {
            val style: Style? = getTextComponentUnderMouse(mouseX.toInt(), mouseY.toInt())
            if (style?.clickEvent != null && style.clickEvent!!.action == ClickEvent.Action.OPEN_URL) {
                handleTextClick(style)
                return false
            }
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
            in 80..89 -> 0
            in 120..129 -> 1
            in 135..144 -> 2
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