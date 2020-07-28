package com.williambl.haema.plugin.rei

import com.williambl.haema.craft.BookOfBloodRecipe
import com.williambl.haema.logger
import me.shedaniel.rei.api.RecipeHelper
import me.shedaniel.rei.api.plugins.REIPluginV0
import net.minecraft.util.Identifier

class HaemaREIPlugin : REIPluginV0 {

    val id = Identifier("haema:haema_rei_plugin")

    override fun getPluginIdentifier(): Identifier = id

    override fun registerRecipeDisplays(recipeHelper: RecipeHelper) {
        logger.info("REI detected. Adding the Book of Blood recipe display.")
        recipeHelper.recipeManager.get(Identifier("haema:book_of_blood")).ifPresent {
            if (it is BookOfBloodRecipe)
                recipeHelper.registerDisplay(BookOfBloodRecipeDisplay(it))
        }
    }
}