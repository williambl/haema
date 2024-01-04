package com.williambl.haema.compat.modmenu

import com.williambl.haema.id
import com.williambl.haema.util.drawCenteredText
import net.minecraft.client.MinecraftClient
import net.minecraft.client.gui.DrawContext
import net.minecraft.client.gui.screen.Screen
import net.minecraft.client.gui.widget.ButtonWidget
import net.minecraft.client.gui.widget.ButtonWidget.PressAction
import net.minecraft.client.util.InputUtil
import net.minecraft.text.ClickEvent
import net.minecraft.text.MutableText
import net.minecraft.text.Style
import net.minecraft.text.Text
import net.minecraft.util.Formatting
import net.minecraft.util.Util
import org.lwjgl.glfw.GLFW
import java.awt.Color
import java.net.URI
import java.net.URISyntaxException
import java.util.function.Supplier

class HaemaGameplayConfigScreen(private val parent: Screen?) : Screen(Text.literal("HAEMA").formatted(Formatting.UNDERLINE)) {
    val icon = id("icon.png")

    var isShowingMore = false

    private val keys = listOf(
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

    private var wasKeyDown = false
    private var currentKey = 0
    private var currentHue = 0.0
    private var isDoingAnim = false

    private val texts = listOf(
        Text.translatable("gui.haema.config.gameplay.main").formatted(Formatting.UNDERLINE),
        Text.translatable("gui.haema.config.gameplay.gameruleslink")
            .setStyle(Style.EMPTY
                .withClickEvent(
                    ClickEvent(ClickEvent.Action.OPEN_URL, "https://minecraft.gamepedia.com/Commands/gamerule")
                )
                .withUnderline(true)
                .withColor(Formatting.BLUE)
            ),
        Text.translatable("gui.haema.config.gameplay.datapackslink")
            .setStyle(Style.EMPTY
                .withClickEvent(
                    ClickEvent(ClickEvent.Action.OPEN_URL, "https://minecraft.gamepedia.com/Data_Pack")
                )
                .withUnderline(true)
                .withColor(Formatting.BLUE)
            )
    )
    private val extratexts = listOf(
        Text.translatable("gui.haema.config.gameplay.bloodsources"),
        Text.literal("haema:good_blood_sources").formatted(Formatting.UNDERLINE),
        Text.literal("haema:medium_blood_sources").formatted(Formatting.UNDERLINE),
        Text.literal("haema:poor_blood_sources").formatted(Formatting.UNDERLINE),

        Text.translatable("gui.haema.config.gameplay.vampireweapons")
            .append(Text.literal("haema:vampire_weapons").formatted(Formatting.UNDERLINE)),

        Text.translatable("gui.haema.config.gameplay.gamerules")
    )

    override fun init() {
        super.init()
        addDrawableChild(object : ButtonWidget(width/2-210, 180, 200, 20, Text.translatable("gui.haema.moreinfo"), PressAction {
            isShowingMore = !isShowingMore
        }, Supplier<MutableText>::get) {
            override fun getMessage(): Text {
                return Text.translatable(if (isShowingMore) "gui.haema.lessinfo" else "gui.haema.moreinfo")
            }
        })
        addDrawableChild(ButtonWidget.builder(Text.translatable("gui.done")) {
            close()
        }.dimensions(width/2+10, 180, 200, 20).build())
    }

    override fun render(context: DrawContext, mouseX: Int, mouseY: Int, delta: Float) {
        renderBackground(context)
        super.render(context, mouseX, mouseY, delta)
        context.drawTexture(
            icon,
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
            context.drawCenteredText(textRenderer, extratexts[0], width / 2, 80, 0xffffff)
            context.drawCenteredText(textRenderer, extratexts[1], width / 2, 90, 0xffffff)
            context.drawCenteredText(textRenderer, extratexts[2], width / 2, 100, 0xffffff)
            context.drawCenteredText(textRenderer, extratexts[3], width / 2, 110, 0xffffff)
            context.drawCenteredText(textRenderer, extratexts[4], width / 2, 130, 0xffffff)
            context.drawCenteredText(textRenderer, extratexts[5], width / 2, 150, 0xffffff)
        } else {
            context.drawCenteredText(textRenderer, texts[0], width / 2, 80, if (isDoingAnim) Color.HSBtoRGB(currentHue.toFloat(), 0.8f, 0.8f) else 0xffffff)
            context.drawCenteredText(textRenderer, texts[1], width / 2, 120, 0xffffff)
            context.drawCenteredText(textRenderer, texts[2], width / 2, 135, 0xffffff)
        }
    }

    override fun close() {
        client?.setScreen(parent)
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