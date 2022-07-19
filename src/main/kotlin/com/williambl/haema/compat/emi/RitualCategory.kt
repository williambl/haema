package com.williambl.haema.compat.emi

import com.williambl.haema.id
import com.williambl.haema.ritual.RitualModule
import dev.emi.emi.api.recipe.EmiRecipeCategory
import dev.emi.emi.api.render.EmiRenderable
import dev.emi.emi.api.stack.EmiStack
import net.minecraft.util.Identifier

object RitualCategory: EmiRecipeCategory(id("ritual"), EmiStack.of(RitualModule.RITUAL_TABLE_ITEM)) {
}