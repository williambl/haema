package com.williambl.haema.compat.rei

import com.williambl.haema.ritual.RitualTable
import me.shedaniel.math.Point
import me.shedaniel.math.Rectangle
import me.shedaniel.rei.api.client.gui.Renderer
import me.shedaniel.rei.api.client.gui.widgets.Widget
import me.shedaniel.rei.api.client.gui.widgets.Widgets
import me.shedaniel.rei.api.client.registry.display.DisplayCategory
import me.shedaniel.rei.api.common.category.CategoryIdentifier
import me.shedaniel.rei.api.common.util.EntryStacks
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.util.Util

class RitualCategory: DisplayCategory<RitualDisplay> {
    override fun getCategoryIdentifier(): CategoryIdentifier<out RitualDisplay> = HaemaREIPlugin.ritualId

    override fun getTitle(): Text = TranslatableText("rei.${identifier.toString().replace(':', '.')}")

    override fun setupDisplay(recipeDisplay: RitualDisplay, bounds: Rectangle): MutableList<Widget> {
        val inputsPoint = Point(bounds.centerX - 64, bounds.centerY - 16)
        val outputPoint = Point(bounds.centerX + 26, bounds.centerY - 8)
        val widgets = mutableListOf<Widget>()

        widgets.add(Widgets.createRecipeBase(bounds))

        val inputEntries = recipeDisplay.inputEntries
        widgets.addAll(inputEntries.mapIndexed { i, stacks ->
            Widgets.createSlot(Point(inputsPoint.x+((i+1)%2)*18, inputsPoint.y+(i/2)*18)).entries(stacks)
        })

        widgets.add(Widgets.createLabel(Point(bounds.centerX, bounds.centerY-28),
            (TranslatableText("gui.haema.requires").append(TranslatableText("gui.haema.altar_level."+recipeDisplay.recipe.minLevel))).formatted(Formatting.UNDERLINE).formatted(Formatting.DARK_GRAY)
        ).noShadow())

        widgets.add(Widgets.createLabel(Point(bounds.centerX+16, bounds.centerY+16), TranslatableText("gui.haema.repeatable.${recipeDisplay.recipe.isRepeatable}").formatted(if (recipeDisplay.recipe.isRepeatable) Formatting.DARK_GREEN else Formatting.DARK_RED)).noShadow())

        widgets.add(Widgets.createLabel(outputPoint, TranslatableText(Util.createTranslationKey("ritual_action", recipeDisplay.recipe.actionName), recipeDisplay.recipe.actionArg).formatted(Formatting.DARK_GRAY)).noShadow())

        return widgets
    }

    override fun getIcon(): Renderer = EntryStacks.of(RitualTable.instance.asItem().defaultStack)
}