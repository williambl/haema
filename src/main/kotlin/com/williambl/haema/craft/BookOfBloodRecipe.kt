package com.williambl.haema.craft

import net.minecraft.inventory.CraftingInventory
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions
import net.minecraft.recipe.RecipeSerializer
import net.minecraft.recipe.SpecialCraftingRecipe
import net.minecraft.recipe.SpecialRecipeSerializer
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry
import net.minecraft.world.World

class BookOfBloodRecipe(id: Identifier?) : SpecialCraftingRecipe(id) {
    val resultStack = ItemStack(Registry.ITEM[Identifier("haema:book_of_blood")])

    override fun craft(inv: CraftingInventory): ItemStack {
        var foundBook = false
        var foundPotion = false
        for (i in 0 until inv.size()) {
            val stack = inv.getStack(i)
            if (stack.isEmpty) continue
            if (foundBook && foundPotion) return ItemStack.EMPTY
            when (stack.item) {
                Items.BOOK -> {
                    if (foundBook) return ItemStack.EMPTY
                    foundBook = true
                }
                Items.POTION -> {
                    if (foundPotion) return ItemStack.EMPTY
                    if (PotionUtil.getPotion(stack) == Potions.AWKWARD) {
                        foundPotion = true
                    }
                }
                else -> return ItemStack.EMPTY
            }
        }

        return if (foundBook && foundPotion) resultStack.copy()
        else ItemStack.EMPTY
    }

    override fun fits(width: Int, height: Int): Boolean = width * height >= 2

    override fun getSerializer(): RecipeSerializer<*> = Serializer

    override fun matches(inv: CraftingInventory, world: World?): Boolean {
        var foundBook = false
        var foundPotion = false
        for (i in 0 until inv.size()) {
            val stack = inv.getStack(i)
            if (stack.isEmpty) continue
            if (foundBook && foundPotion) return false
            if (stack.item == Items.BOOK) {
                if (foundBook) return false
                foundBook = true
            }
            if (stack.item == Items.POTION) {
                if (foundPotion) return false
                if (PotionUtil.getPotion(stack) == Potions.AWKWARD) {
                    foundPotion = true
                }
            }
        }

        return foundBook && foundPotion
    }

    companion object Serializer: SpecialRecipeSerializer<BookOfBloodRecipe>({ id -> BookOfBloodRecipe(id) })
}