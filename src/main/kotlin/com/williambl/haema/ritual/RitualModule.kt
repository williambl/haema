package com.williambl.haema.ritual

import com.williambl.haema.id
import com.williambl.haema.ritual.craft.RitualRecipe
import com.williambl.haema.util.MultiTagMatcher
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.tag.TagFactory
import net.minecraft.block.Block
import net.minecraft.item.BlockItem
import net.minecraft.item.Item
import net.minecraft.item.ItemGroup
import net.minecraft.recipe.RecipeSerializer
import net.minecraft.recipe.RecipeType
import net.minecraft.state.property.Properties
import net.minecraft.tag.Tag
import net.minecraft.util.registry.Registry
import vazkii.patchouli.common.multiblock.DenseMultiblock
import vazkii.patchouli.common.multiblock.MultiblockRegistry
import vazkii.patchouli.common.multiblock.StateMatcher

object RitualModule: ModInitializer {
    val RITUAL_RECIPE_TYPE: RecipeType<RitualRecipe> = RecipeType.register("haema:ritual")
    val RITUAL_RECIPE_SERIALIZER: RitualRecipe.Companion.Serializer = RecipeSerializer.register("haema:ritual",
        RitualRecipe.Companion.Serializer
    )

    val RITUAL_TABLE_BLOCK: RitualTable = Registry.register(
            Registry.BLOCK,
            id("ritual_table"),
            RitualTable.instance
        )
    val RITUAL_TABLE_ITEM: BlockItem = Registry.register(
            Registry.ITEM,
            id("ritual_table"),
            BlockItem(RitualTable.instance, Item.Settings().group(ItemGroup.DECORATIONS))
        )

    val LEVEL_0_RITUAL_MATERIALS: Tag<Block> = TagFactory.BLOCK.create(id("ritual_materials/level_0"))
    val LEVEL_1_RITUAL_MATERIALS: Tag<Block> = TagFactory.BLOCK.create(id("ritual_materials/level_1"))

    val LEVEL_0_RITUAL_TORCHES: Tag<Block> = TagFactory.BLOCK.create(id("ritual_torches/level_0"))
    val LEVEL_1_RITUAL_TORCHES: Tag<Block> = TagFactory.BLOCK.create(id("ritual_torches/level_1"))

    override fun onInitialize() {
        MultiblockRegistry.registerMultiblock(
            id("basic_altar"), DenseMultiblock(
                arrayOf(
                    arrayOf(
                        "T T T",
                        "     ",
                        "T   T",
                        "     ",
                        "T T T"
                    ), arrayOf(
                        "B B B",
                        "     ",
                        "B 0 B",
                        "     ",
                        "B B B"
                    ), arrayOf(
                        "BBBBB",
                        "B   B",
                        "B B B",
                        "B   B",
                        "BBBBB"
                    ), arrayOf(
                        "BBBBB",
                        "BBBBB",
                        "BBBBB",
                        "BBBBB",
                        "BBBBB"
                    )
                ), mapOf(
                    'T' to MultiTagMatcher(
                        listOf(LEVEL_0_RITUAL_TORCHES as Tag.Identified<Block>),
                        mapOf(Properties.LIT to true)
                    ),
                    'B' to MultiTagMatcher(listOf(LEVEL_0_RITUAL_MATERIALS as Tag.Identified<Block>), mapOf()),
                    '0' to StateMatcher.fromBlockLoose(RitualTable.instance),
                    ' ' to StateMatcher.ANY
                )
            )
        )

        MultiblockRegistry.registerMultiblock(
            id("blackstone_altar"), DenseMultiblock(
                arrayOf(
                    arrayOf(
                        "T T T",
                        "     ",
                        "T   T",
                        "     ",
                        "T T T"
                    ), arrayOf(
                        "B B B",
                        "     ",
                        "B 0 B",
                        "     ",
                        "B B B"
                    ), arrayOf(
                        "BBBBB",
                        "B   B",
                        "B B B",
                        "B   B",
                        "BBBBB"
                    ), arrayOf(
                        "BBBBB",
                        "BBBBB",
                        "BBBBB",
                        "BBBBB",
                        "BBBBB"
                    )
                ), mapOf(
                    'T' to MultiTagMatcher(
                        listOf(LEVEL_1_RITUAL_TORCHES as Tag.Identified<Block>),
                        mapOf(Properties.LIT to true)
                    ),
                    'B' to MultiTagMatcher(listOf(LEVEL_1_RITUAL_MATERIALS as Tag.Identified<Block>), mapOf()),
                    '0' to StateMatcher.fromBlockLoose(RitualTable.instance),
                    ' ' to StateMatcher.ANY
                )
            )
        )
    }
}