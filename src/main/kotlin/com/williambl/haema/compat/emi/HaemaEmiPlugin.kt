package com.williambl.haema.compat.emi

import com.williambl.haema.api.RitualTableUseEvent
import com.williambl.haema.craft.BookOfBloodRecipe
import com.williambl.haema.ritual.RitualModule
import dev.emi.emi.api.EmiApi
import dev.emi.emi.api.EmiPlugin
import dev.emi.emi.api.EmiRegistry
import dev.emi.emi.api.recipe.EmiCraftingRecipe
import dev.emi.emi.api.stack.EmiIngredient
import dev.emi.emi.api.stack.EmiStack
import net.minecraft.item.Items
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions
import net.minecraft.recipe.RecipeType
import vazkii.patchouli.common.item.PatchouliItems

class HaemaEmiPlugin: EmiPlugin{
    override fun register(registry: EmiRegistry) {
        registry.addCategory(RitualCategory)

        val manager = registry.recipeManager

        for (recipe in manager.listAllOfType(RitualModule.RITUAL_RECIPE_TYPE)) {
            registry.addRecipe(RitualEmiRecipe(recipe))
        }

        for (recipe in manager.listAllOfType(RecipeType.CRAFTING)) {
            if (recipe is BookOfBloodRecipe) {
                registry.addRecipe(EmiCraftingRecipe(
                    listOf(
                        EmiStack.of(Items.BOOK),
                        EmiStack.of(Items.POTION.defaultStack.also { PotionUtil.setPotion(it, Potions.AWKWARD) })
                    ),
                    EmiStack.of(recipe.resultStack),
                    recipe.id,
                    true
                ))
            }
        }
    }

    init {
        RitualTableUseEvent.EVENT.register(RitualTableUseEvent { _, world, _, player, hand, _ ->
            if (player.getStackInHand(hand).item == PatchouliItems.BOOK && world.isClient) {
                EmiApi.displayRecipeCategory(RitualCategory)
            }
        })
    }
}