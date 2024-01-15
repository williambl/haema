package com.williambl.haema.client.gui

import com.mojang.blaze3d.systems.RenderSystem
import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.VampireAbility
import com.williambl.haema.api.AbilityVisibilityEvent
import com.williambl.haema.ritual.RitualTableScreenHandler
import net.minecraft.advancement.AdvancementFrame
import net.minecraft.client.gui.DrawContext
import net.minecraft.client.gui.screen.advancement.AdvancementObtainedStatus
import net.minecraft.client.gui.screen.ingame.HandledScreen
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.text.Text
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier
import kotlin.math.max

class RitualTableScreen(handler: RitualTableScreenHandler, inventory: PlayerInventory, title: Text) :
    HandledScreen<RitualTableScreenHandler>(handler, inventory, title) {

    private val BACKGROUND_TEXTURE = Identifier("minecraft:textures/gui/advancements/backgrounds/stone.png");
    private val widgets = AbilityModule.ABILITY_REGISTRY.entrySet.asSequence()
        .filterNot { it.value == AbilityModule.NONE }
        .filter { AbilityVisibilityEvent.EVENT.invoker().onVisibilityTest(inventory.player, it.value).orElse(it.value.isVisible(inventory.player)) }
        .map { val ids = Pair(it.key.value, AbilityModule.ABILITY_REGISTRY.getRawId(it.value)); ids to List(it.value.maxLevel) { idx -> AbilityWidget(ids, it.value, idx + 1) } }
        .toList()

    private var movingTab = false

    // There is 4 px padding between widgets and 3 px around the widgets.
    // Widgets are 26x26 pixels so the 30 includes 4 pixes of padding at the end, which is why we also subtract 1.
    private var panX = max(234 - (3 + 30 * widgets.size - 1), 0) / 2.0
    private var panY = max(113 - (3 + 30 * widgets.maxOf { it.second.size } - 1), 0) / 2.0

    override fun drawBackground(context: DrawContext, delta: Float, mouseX: Int, mouseY: Int) {}

    override fun render(context: DrawContext, mouseX: Int, mouseY: Int, delta: Float) {
        val xBase = (width - 252) / 2
        val yBase = (height - 140) / 2

        renderWindow(context, xBase, yBase)
        renderWindowBorder(context, xBase, yBase)
        renderWidgetTooltip(context, xBase, yBase, mouseX, mouseY)
    }

    private fun renderWindow(context: DrawContext, xBase: Int, yBase: Int) {
        val xWin = xBase + 9
        val yWin = yBase + 18

        context.enableScissor(xWin, yWin, xWin + 234, yWin + 113)
        context.matrices.push()
        context.matrices.translate(xWin.toFloat(), yWin.toFloat(), 0.0f)

        val k = panX.toInt() % 16
        val l = panY.toInt() % 16

        for (m in -1..15) {
            for (n in -1..8) {
                context.drawTexture(BACKGROUND_TEXTURE, k + 16 * m, l + 16 * n, 0.0f, 0.0f, 16, 16, 16, 16)
            }
        }

        widgets.forEachIndexed { i, widgets ->
            widgets.second.forEachIndexed { j, widget ->
                widget.render(
                    context,
                    (panX + 3 + i * 30).toInt(),
                    (panY + 3 + j * 30).toInt(),
                    handler.getProperty(widgets.first.second)
                )
            }
        }

        context.matrices.pop()
        context.disableScissor()
    }

    private fun renderWindowBorder(context: DrawContext, xBase: Int, yBase: Int) {
        context.matrices.push()
        RenderSystem.setShaderColor(1.0f, 1.0f, 1.0f, 1.0f)
        RenderSystem.enableBlend()
        context.drawTexture(WINDOW_TEXTURE, xBase, yBase, 0, 0, 252, 140)

        val pointsRemaining = handler.getProperty(0)
        context.drawTextWithShadow(
            this.textRenderer,
            Text.literal("Ability Points remaining: ")
                .append(
                    Text.literal(pointsRemaining.toString())
                        .formatted(if (pointsRemaining == 0) Formatting.RED else Formatting.GREEN)
                ),
            xBase + 6,
            yBase + 6,
            0xffffff
        )
        context.matrices.pop()
    }

    private fun renderWidgetTooltip(context: DrawContext, xBase: Int, yBase: Int, mouseX: Int, mouseY: Int) {
        val widget = getWidgetHovered(xBase, yBase, mouseX, mouseY)

        if (widget != null) {
            context.drawTooltip(
                this.textRenderer,
                listOf(
                    Text.translatable("ability.${widget.abilityIds.first.namespace}.${widget.abilityIds.first.path}").append(" ${widget.level}")
                        .formatted(Formatting.BOLD).formatted(Formatting.UNDERLINE),
                    Text.translatable("ability.${widget.abilityIds.first.namespace}.${widget.abilityIds.first.path}.description")
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

    override fun close() {
        super.close()
        handler.onClosed(handler.inv.player)
    }

    private fun getWidgetHovered(xBase: Int, yBase: Int, mouseX: Int, mouseY: Int): AbilityWidget? {
        val xWin = xBase + 9
        val yWin = yBase + 18

        widgets.forEachIndexed { i, widgets ->
            widgets.second.forEachIndexed { j, widget ->
                // Widgets below max level are not as wide.
                val wShrink = if (widget.level < widget.ability.maxLevel) 2 else 0
                val xMin = xWin + panX.toInt() + 3 + i * 30 + wShrink
                val yMin = yWin + panY.toInt() + 3 + j * 30
                val xMax = xMin + 25 - wShrink * 2
                val yMax = yMin + 25

                if (mouseX in xMin..xMax && mouseY in yMin..yMax) {
                    return widget
                }
            }
        }
        return null
    }

    class AbilityWidget(val abilityIds: Pair<Identifier, Int>, val ability: VampireAbility, val level: Int) {
        fun render(
            context: DrawContext,
            x: Int,
            y: Int,
            levelAchieved: Int
        ) {
            context.drawTexture(
                WIDGETS_TEXTURE,
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

            context.drawItemWithoutEntity(
                ability.iconItem,
                x + 5,
                y + 5
            )
        }
    }

    companion object {
        private val WINDOW_TEXTURE = Identifier("textures/gui/advancements/window.png")
        private val WIDGETS_TEXTURE = Identifier("textures/gui/advancements/widgets.png")
    }
}