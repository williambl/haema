package com.williambl.haema.plugin.rei

import com.williambl.haema.craft.BookOfBloodRecipe
import me.shedaniel.rei.api.EntryStack
import me.shedaniel.rei.plugin.crafting.DefaultCraftingDisplay
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.nbt.CompoundTag
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions
import net.minecraft.recipe.Recipe
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry
import java.util.*

class BookOfBloodRecipeDisplay(private val recipe: BookOfBloodRecipe) : DefaultCraftingDisplay {

    private val resultStack = ItemStack(Registry.ITEM[Identifier("patchouli:guide_book")])

    init {
        val tag = CompoundTag()
        tag.putString("patchouli:book", "haema:book_of_blood")
        resultStack.tag = tag
    }

    override fun getOptionalRecipe(): Optional<Recipe<*>> = Optional.of(recipe)

    override fun getInputEntries(): MutableList<MutableList<EntryStack>> {
        val potionStack = ItemStack(Items.POTION)
        PotionUtil.setPotion(potionStack, Potions.AWKWARD)

        return mutableListOf(
            EntryStack.ofItemStacks(mutableListOf(ItemStack(Items.BOOK))),
            EntryStack.ofItemStacks(mutableListOf(potionStack))
        )
    }

    override fun getOutputEntries(): MutableList<EntryStack> = EntryStack.ofItemStacks(listOf(resultStack.copy()))

    override fun getRecipeLocation(): Optional<Identifier> = optionalRecipe.map { it.id }

    override fun getRequiredEntries(): MutableList<MutableList<EntryStack>> = inputEntries

    override fun getWidth(): Int = 2

    override fun getHeight(): Int = 2
}