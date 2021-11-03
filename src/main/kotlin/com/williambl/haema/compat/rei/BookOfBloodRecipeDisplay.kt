package com.williambl.haema.compat.rei

import com.williambl.haema.craft.BookOfBloodRecipe
import me.shedaniel.rei.api.common.entry.EntryIngredient
import me.shedaniel.rei.api.common.util.EntryIngredients
import me.shedaniel.rei.plugin.common.displays.crafting.DefaultCraftingDisplay
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions
import java.util.*

class BookOfBloodRecipeDisplay(recipe: BookOfBloodRecipe) : DefaultCraftingDisplay<BookOfBloodRecipe>(
    makeInputs(recipe), makeOutputs(recipe), Optional.of(recipe)) {
    companion object {
        fun makeInputs(recipe: BookOfBloodRecipe): List<EntryIngredient> {
            val potionStack = ItemStack(Items.POTION)
            PotionUtil.setPotion(potionStack, Potions.AWKWARD)

            return listOf(
                EntryIngredients.ofItemStacks(listOf(ItemStack(Items.BOOK))),
                EntryIngredients.ofItemStacks(listOf(potionStack))
            )
        }

        fun makeOutputs(recipe: BookOfBloodRecipe): List<EntryIngredient> {
            return listOf(EntryIngredients.of(recipe.resultStack))
        }
    }

    override fun getWidth(): Int = 2

    override fun getHeight(): Int = 2
}
