package com.williambl.haema.client.gui

import com.mojang.blaze3d.systems.RenderSystem
import com.williambl.haema.VampireAbility
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
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier

class RitualTableScreen(handler: RitualTableScreenHandler, inventory: PlayerInventory, title: Text) :
    HandledScreen<RitualTableScreenHandler>(handler, inventory, title) {

    private val widgets = VampireAbility.values()
        .filterNot { it == VampireAbility.NONE }
        .map { List (it.maxLevel) { idx -> AbilityWidget(it, idx+1) } }

    private var movingTab = false

    var panX = 117 / 2.0
    var panY = 56 / 2.0

    override fun drawBackground(matrices: MatrixStack?, delta: Float, mouseX: Int, mouseY: Int) {}

    override fun render(matrices: MatrixStack, mouseX: Int, mouseY: Int, delta: Float) {
        val xBase = (width - 252) / 2
        val yBase = (height - 140) / 2

        renderWindow(matrices, xBase, yBase)
        renderWindowBorder(matrices, xBase, yBase)
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
            widgets.forEachIndexed { j, widget ->
                widget.render(matrices, (panX+3+i*30).toInt(), (panY+3+j*30).toInt(), xBase, yBase, client!!, handler.getProperty(i+1))
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
                .append(LiteralText(pointsRemaining.toString())
                    .formatted(if (pointsRemaining == 0) Formatting.RED else Formatting.GREEN)),
            xBase + 6,
            yBase + 6,
            0xffffff
        )
        matrices.pop()
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

    class AbilityWidget(val ability: VampireAbility, val level: Int): DrawableHelper() {
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