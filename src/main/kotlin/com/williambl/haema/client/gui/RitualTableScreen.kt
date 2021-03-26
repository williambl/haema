package com.williambl.haema.client.gui

import com.mojang.blaze3d.systems.RenderSystem
import com.williambl.haema.abilities.VampireAbility
import com.williambl.haema.abilities.abilityRegistry
import com.williambl.haema.abilities.noneIdentifier
import com.williambl.haema.ritual.RitualTableScreenHandler
import net.minecraft.advancement.AdvancementFrame
import net.minecraft.client.MinecraftClient
import net.minecraft.client.gui.DrawableHelper
import net.minecraft.client.gui.screen.advancement.AdvancementObtainedStatus
import net.minecraft.client.gui.screen.ingame.HandledScreen
import net.minecraft.client.util.math.MatrixStack
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.text.LiteralText
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier
import java.util.*

class RitualTableScreen(handler: RitualTableScreenHandler, inventory: PlayerInventory, title: Text) :
    HandledScreen<RitualTableScreenHandler>(handler, inventory, title) {

    private val widgets = abilityRegistry.entries.asSequence()
        .filterNot { it.value == VampireAbility.NONE }
        .filter { it.value.isVisible(inventory.player) }
        .map { val ids = Pair(it.key.value, abilityRegistry.getRawId(it.value)); ids to List(it.value.maxLevel) { idx -> AbilityWidget(ids, it.value, idx + 1) } }
        .toList()

    private var movingTab = false

    private var panX = 117 / 2.0
    private var panY = 56 / 2.0

    override fun drawBackground(matrices: MatrixStack?, delta: Float, mouseX: Int, mouseY: Int) {}

    override fun render(matrices: MatrixStack, mouseX: Int, mouseY: Int, delta: Float) {
        val xBase = (width - 252) / 2
        val yBase = (height - 140) / 2

        renderWindow(matrices, xBase, yBase)
        renderWindowBorder(matrices, xBase, yBase)
        renderWidgetTooltip(matrices, xBase, yBase, mouseX, mouseY)
    }

    private fun renderWindow(matrices: MatrixStack, xBase: Int, yBase: Int) {
        matrices.push()
        RenderSystem.enableDepthTest()
        matrices.translate(((xBase + 9).toDouble()), ((yBase + 18).toDouble()), 950.0)
        RenderSystem.colorMask(false, false, false, false)
        fill(matrices, 4680, 2260, -4680, -2260, -16777216)
        RenderSystem.colorMask(true, true, true, true)
        matrices.translate(0.0, 0.0, -950.0)
        RenderSystem.depthFunc(518)
        fill(matrices, 234, 113, 0, 0, -16777216)
        RenderSystem.depthFunc(515)
        val identifier = Identifier("minecraft:textures/gui/advancements/backgrounds/stone.png")
        client!!.textureManager.bindTexture(identifier)

        val k = panX.toInt() % 16
        val l = panY.toInt() % 16

        for (m in -1..15) {
            for (n in -1..8) {
                drawTexture(matrices, k + 16 * m, l + 16 * n, 0.0f, 0.0f, 16, 16, 16, 16)
            }
        }

        widgets.forEachIndexed { i, widgets ->
            widgets.second.forEachIndexed { j, widget ->
                widget.render(
                    matrices,
                    (panX + 3 + i * 30).toInt(),
                    (panY + 3 + j * 30).toInt(),
                    xBase,
                    yBase,
                    client!!,
                    handler.getProperty(widgets.first.second)
                )
            }
        }

        RenderSystem.depthFunc(518)
        matrices.translate(0.0, 0.0, -950.0)
        RenderSystem.colorMask(false, false, false, false)
        fill(matrices, 4680, 2260, -4680, -2260, -16777216)
        RenderSystem.colorMask(true, true, true, true)
        matrices.translate(0.0, 0.0, 950.0)
        RenderSystem.depthFunc(515)
        RenderSystem.disableDepthTest()
        matrices.pop()
    }

    private fun renderWindowBorder(matrices: MatrixStack, xBase: Int, yBase: Int) {
        matrices.push()
        RenderSystem.color4f(1.0f, 1.0f, 1.0f, 1.0f)
        RenderSystem.enableBlend()
        client!!.textureManager.bindTexture(WINDOW_TEXTURE)
        drawTexture(matrices, xBase, yBase, 0, 0, 252, 140)

        val pointsRemaining = handler.getProperty(0)
        drawTextWithShadow(
            matrices,
            client!!.textRenderer,
            LiteralText("Ability Points remaining: ")
                .append(
                    LiteralText(pointsRemaining.toString())
                        .formatted(if (pointsRemaining == 0) Formatting.RED else Formatting.GREEN)
                ),
            xBase + 6,
            yBase + 6,
            0xffffff
        )
        matrices.pop()
    }

    private fun renderWidgetTooltip(matrices: MatrixStack, xBase: Int, yBase: Int, mouseX: Int, mouseY: Int) {
        val widget = getWidgetHovered(xBase, yBase, mouseX, mouseY)

        if (widget != null) {
            renderTooltip(
                matrices, listOf(
                    TranslatableText("ability.${widget.abilityIds.first.namespace}.${widget.abilityIds.first.path}").append(" ${widget.level}")
                        .formatted(Formatting.BOLD).formatted(Formatting.UNDERLINE),
                    TranslatableText("ability.${widget.abilityIds.first.namespace}.${widget.abilityIds.first.path}.description")
                ), mouseX, mouseY
            )
        }
    }

    override fun mouseClicked(mouseX: Double, mouseY: Double, button: Int): Boolean {
        if (button != 0) return super.mouseClicked(mouseX, mouseY, button)

        val xBase = (width - 252) / 2
        val yBase = (height - 140) / 2

        val widget = getWidgetHovered(xBase, yBase, mouseX.toInt(), mouseY.toInt())

        if (widget != null) {
            val currentLevel = handler.getProperty(widget.abilityIds.second)
            val spareLevels = handler.getProperty(0)
            val widgetLevel = widget.level

            if (widgetLevel > currentLevel) {
                if (widgetLevel - currentLevel <= spareLevels) {
                    handler.transferLevels(widgetLevel - currentLevel, 0, widget.abilityIds.second)
                }
            } else if (widgetLevel < currentLevel) {
                handler.transferLevels(currentLevel - widgetLevel, widget.abilityIds.second, 0)
            } else if (widgetLevel == currentLevel) {
                handler.transferLevels(1, widget.abilityIds.second, 0)
            }
        }
        return super.mouseClicked(mouseX, mouseY, button)
    }

    override fun mouseDragged(mouseX: Double, mouseY: Double, button: Int, deltaX: Double, deltaY: Double): Boolean {
        return if (button != 0) {
            movingTab = false
            false
        } else {
            if (!movingTab) {
                movingTab = true
            } else {
                panX += deltaX
                panY += deltaY
            }
            true
        }
    }

    override fun onClose() {
        super.onClose()
        handler.close(handler.inv.player)
    }

    private fun getWidgetHovered(xBase: Int, yBase: Int, mouseX: Int, mouseY: Int): AbilityWidget? {
        for (i in widgets.indices) for (j in widgets[i].second.indices) {
            x = xBase + panX.toInt() + 15 + i * 30
            y = yBase + panY.toInt() + 20 + j * 30

            if (mouseX in x..x+22 && mouseY in y..y+26) {
                return widgets[i].second[j]
            }
        }
        return null
    }

    class AbilityWidget(val abilityIds: Pair<Identifier, Int>, val ability: VampireAbility, val level: Int): DrawableHelper() {
        fun render(
            matrices: MatrixStack,
            x: Int,
            y: Int,
            xBase: Int,
            yBase: Int,
            client: MinecraftClient,
            levelAchieved: Int
        ) {
            client.textureManager.bindTexture(WIDGETS_TEXTURE)
            drawTexture(
                matrices,
                x,
                y,
                if (level == ability.maxLevel)
                    AdvancementFrame.CHALLENGE.textureV
                else
                    AdvancementFrame.GOAL.textureV,
                128 + (
                        if (levelAchieved >= level)
                            AdvancementObtainedStatus.OBTAINED.spriteIndex
                        else
                            AdvancementObtainedStatus.UNOBTAINED.spriteIndex
                        )
                        * 26,
                26,
                26
            )

            client.itemRenderer.renderInGui(
                ability.iconItem,
                x + xBase + 13,
                y + yBase + 22
            )
        }
    }

    companion object {
        private val WINDOW_TEXTURE = Identifier("textures/gui/advancements/window.png")
        private val WIDGETS_TEXTURE = Identifier("textures/gui/advancements/widgets.png")
    }
}