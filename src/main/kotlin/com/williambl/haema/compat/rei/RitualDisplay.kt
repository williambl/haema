package com.williambl.haema.compat.rei

import com.williambl.haema.ritual.craft.RitualRecipe
import me.shedaniel.rei.api.common.category.CategoryIdentifier
import me.shedaniel.rei.api.common.display.Display
import me.shedaniel.rei.api.common.entry.EntryIngredient
import me.shedaniel.rei.api.common.util.EntryIngredients

class RitualDisplay(val recipe: RitualRecipe) : Display {
    private val input: MutableList<EntryIngredient> = EntryIngredients.ofIngredients(recipe.ingredients).toMutableList()

    init {
        input.add(EntryIngredients.of(recipe.fluid))
    }

    override fun getInputEntries(): List<EntryIngredient> = input

    override fun getOutputEntries(): List<EntryIngredient> = emptyList()

    override fun getCategoryIdentifier(): CategoryIdentifier<*> = HaemaREIPlugin.ritualId
}
