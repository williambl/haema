package com.williambl.haema.ritual

import com.williambl.haema.Haema
import com.williambl.haema.api.RitualTableUseEvent
import com.williambl.haema.id
import com.williambl.haema.ritual.craft.AddLevelsRitualAction
import com.williambl.haema.ritual.craft.ChangeAbilitiesRitualAction
import com.williambl.haema.ritual.craft.RitualAction
import com.williambl.haema.ritual.craft.RitualRecipe
import com.williambl.haema.util.MultiTagMatcher
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder
import net.fabricmc.fabric.api.screenhandler.v1.ExtendedScreenHandlerType
import net.fabricmc.loader.api.FabricLoader
import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.BlockItem
import net.minecraft.item.Item
import net.minecraft.recipe.RecipeSerializer
import net.minecraft.recipe.RecipeType
import net.minecraft.state.property.Properties
import net.minecraft.tag.TagKey
import net.minecraft.text.Text
import net.minecraft.util.Hand
import net.minecraft.util.hit.BlockHitResult
import net.minecraft.util.math.BlockPos
import net.minecraft.util.registry.Registry
import net.minecraft.world.World
import vazkii.patchouli.common.item.PatchouliItems
import vazkii.patchouli.common.multiblock.DenseMultiblock
import vazkii.patchouli.common.multiblock.MultiblockRegistry
import vazkii.patchouli.common.multiblock.StateMatcher

object RitualModule: ModInitializer {
    val RITUAL_ACTION_REGISTRY: Registry<RitualAction> = FabricRegistryBuilder.createSimple(RitualAction::class.java, id("ritual_action")).buildAndRegister()
    val ADD_LEVELS_RITUAL_ACTION: RitualAction = Registry.register(RITUAL_ACTION_REGISTRY, id("add_levels"), AddLevelsRitualAction)
    val CHANGE_ABILITIES_RITUAL_ACTION: RitualAction = Registry.register(RITUAL_ACTION_REGISTRY, id("change_abilities"), ChangeAbilitiesRitualAction)

    val RITUAL_RECIPE_TYPE: RecipeType<RitualRecipe> = RecipeType.register("haema:ritual")
    val RITUAL_RECIPE_SERIALIZER: RitualRecipe.Companion.Serializer = RecipeSerializer.register("haema:ritual",
        RitualRecipe.Companion.Serializer
    )

    val RITUAL_TABLE_SCREEN_HANDLER = Registry.register(Registry.SCREEN_HANDLER, id("ritual_table"), ExtendedScreenHandlerType(::RitualTableScreenHandler))

    val RITUAL_TABLE_BLOCK: RitualTable = Registry.register(
            Registry.BLOCK,
            id("ritual_table"),
            RitualTable.instance
        )
    val RITUAL_TABLE_ITEM: BlockItem = Registry.register(
            Registry.ITEM,
            id("ritual_table"),
            BlockItem(RitualTable.instance, Item.Settings().group(Haema.ITEM_GROUP))
        )

    val LEVEL_0_RITUAL_MATERIALS: TagKey<Block> = TagKey.of(Registry.BLOCK_KEY, id("ritual_materials/level_0"))
    val LEVEL_1_RITUAL_MATERIALS: TagKey<Block> = TagKey.of(Registry.BLOCK_KEY, id("ritual_materials/level_1"))

    val LEVEL_0_RITUAL_TORCHES: TagKey<Block> = TagKey.of(Registry.BLOCK_KEY, id("ritual_torches/level_0"))
    val LEVEL_1_RITUAL_TORCHES: TagKey<Block> = TagKey.of(Registry.BLOCK_KEY, id("ritual_torches/level_1"))

    override fun onInitialize() {
        RitualTableUseEvent.EVENT.register { _: BlockState, world: World, _: BlockPos, player: PlayerEntity, hand: Hand, _: BlockHitResult ->
            if (player.getStackInHand(hand).item == PatchouliItems.BOOK && world.isClient && !FabricLoader.getInstance().isModLoaded("roughlyenoughitems") && !FabricLoader.getInstance().isModLoaded("emi")) {
                player.sendMessage(Text.translatable("gui.haema.message.no_recipe_viewer"), true);
            }
        }

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
                        listOf(LEVEL_0_RITUAL_TORCHES),
                        mapOf(Properties.LIT to true)
                    ),
                    'B' to MultiTagMatcher(listOf(LEVEL_0_RITUAL_MATERIALS), mapOf()),
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
                        listOf(LEVEL_1_RITUAL_TORCHES),
                        mapOf(Properties.LIT to true)
                    ),
                    'B' to MultiTagMatcher(listOf(LEVEL_1_RITUAL_MATERIALS), mapOf()),
                    '0' to StateMatcher.fromBlockLoose(RitualTable.instance),
                    ' ' to StateMatcher.ANY
                )
            )
        )
    }
}