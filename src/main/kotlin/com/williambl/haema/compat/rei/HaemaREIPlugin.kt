package com.williambl.haema.compat.rei

import com.williambl.haema.Haema
import com.williambl.haema.api.RitualTableUseEvent
import com.williambl.haema.craft.BookOfBloodRecipe
import com.williambl.haema.ritual.craft.RitualRecipe
import com.williambl.haema.util.SelfClosingScreen
import me.shedaniel.rei.api.client.plugins.REIClientPlugin
import me.shedaniel.rei.api.client.registry.category.CategoryRegistry
import me.shedaniel.rei.api.client.registry.display.DisplayRegistry
import me.shedaniel.rei.api.client.view.ViewSearchBuilder
import me.shedaniel.rei.api.common.category.CategoryIdentifier
import me.shedaniel.rei.impl.client.REIRuntimeImpl
import vazkii.patchouli.common.item.PatchouliItems

class HaemaREIPlugin : REIClientPlugin {
    companion object {
        val ritualId: CategoryIdentifier<RitualDisplay> = CategoryIdentifier.of("haema:ritual")
    }

    override fun registerDisplays(registry: DisplayRegistry) {
        Haema.LOGGER.info("REI detected. Adding the Book of Blood & Ritual recipe displays.")
        registry.registerFiller(BookOfBloodRecipe::class.java, ::BookOfBloodRecipeDisplay)
        registry.registerFiller(RitualRecipe::class.java, ::RitualDisplay)
    }

    override fun registerCategories(registry: CategoryRegistry) {
        registry.add(RitualCategory())
    }

    init {
        RitualTableUseEvent.EVENT.register(RitualTableUseEvent { _, world, _, player, hand, _ ->
            if (player.getStackInHand(hand).item == PatchouliItems.BOOK && world.isClient) {
                ViewSearchBuilder.builder().addCategory(ritualId).open()
                REIRuntimeImpl.getInstance().previousScreen = SelfClosingScreen()
            }
        })
    }
}
