package com.williambl.haema.compat.rei

import com.williambl.haema.api.RitualTableUseEvent
import com.williambl.haema.craft.BookOfBloodRecipe
import com.williambl.haema.logger
import com.williambl.haema.ritual.craft.RitualRecipe
import me.shedaniel.rei.api.ClientHelper
import me.shedaniel.rei.api.RecipeHelper
import me.shedaniel.rei.api.plugins.REIPluginV0
import net.minecraft.util.Identifier
import vazkii.patchouli.common.item.PatchouliItems

class HaemaREIPlugin : REIPluginV0 {

    val id = Identifier("haema:haema_rei_plugin")

    override fun getPluginIdentifier(): Identifier = id

    override fun registerRecipeDisplays(recipeHelper: RecipeHelper) {
        logger.info("REI detected. Adding the Book of Blood & Ritual recipe displays.")
        recipeHelper.recipeManager.get(Identifier("haema:book_of_blood")).ifPresent {
            if (it is BookOfBloodRecipe)
                recipeHelper.registerDisplay(BookOfBloodRecipeDisplay(it))
        }
        recipeHelper.recipeManager.listAllOfType(RitualRecipe.recipeType).forEach {
            recipeHelper.registerDisplay(RitualCategory.Display(it))
        }
    }

    override fun registerPluginCategories(recipeHelper: RecipeHelper) {
        recipeHelper.registerCategory(RitualCategory())
    }

    override fun registerOthers(recipeHelper: RecipeHelper) {
        super.registerOthers(recipeHelper)

        RitualTableUseEvent.EVENT.register(RitualTableUseEvent { _, _, _, player, hand, _ ->
            if (player.getStackInHand(hand).item == PatchouliItems.book) {
                ClientHelper.getInstance().openView(ClientHelper.ViewSearchBuilder.builder().addCategory(Identifier("haema:ritual")).fillPreferredOpenedCategory())
            }
        })
    }

}