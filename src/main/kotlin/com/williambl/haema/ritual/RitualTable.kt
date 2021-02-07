package com.williambl.haema.ritual

import com.williambl.haema.craft.ritual.RitualInventory
import com.williambl.haema.craft.ritual.RitualRecipe
import com.williambl.haema.level0RitualMaterialsTag
import com.williambl.haema.level0RitualTorchesTag
import com.williambl.haema.level1RitualMaterialsTag
import com.williambl.haema.level1RitualTorchesTag
import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.entity.EntityType
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.fluid.Fluid
import net.minecraft.fluid.FluidState
import net.minecraft.fluid.Fluids
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.ActionResult
import net.minecraft.util.Hand
import net.minecraft.util.hit.BlockHitResult
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Box
import net.minecraft.util.math.Direction
import net.minecraft.world.World
import kotlin.math.min

class RitualTable(settings: Settings) : Block(settings) {

    override fun onUse(
        state: BlockState,
        world: World,
        pos: BlockPos,
        player: PlayerEntity?,
        hand: Hand,
        hit: BlockHitResult
    ): ActionResult {
        val level = min(checkBaseBlockStates(world, pos), checkTorchBlockStates(world, pos))

        if (!world.isClient && player != null) {
            val inventory = getInventory(world, pos, player, level)

            (world as ServerWorld).server.recipeManager.listAllOfType(RitualRecipe.recipeType)
                .firstOrNull { it.matches(inventory) }
                ?.craft(inventory)
        }
        return ActionResult.SUCCESS
    }

    companion object {
        fun checkBaseBlockStates(world: World, tablePos: BlockPos): Int {
            var result = 4
            val mutable = tablePos.mutableCopy()

            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.DOWN)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.EAST, 2)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.UP)).block))
            mutable.move(Direction.DOWN)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.NORTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.NORTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.UP)).block))
            mutable.move(Direction.DOWN)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.WEST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.WEST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.UP)).block))
            mutable.move(Direction.DOWN)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.WEST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.WEST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.UP)).block))
            mutable.move(Direction.DOWN)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.SOUTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.SOUTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.UP)).block))
            mutable.move(Direction.DOWN)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.SOUTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.SOUTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.UP)).block))
            mutable.move(Direction.DOWN)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.EAST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.EAST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.UP)).block))
            mutable.move(Direction.DOWN)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.EAST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.EAST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.UP)).block))
            mutable.move(Direction.DOWN)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.NORTH)).block))

            mutable.set(tablePos).move(Direction.DOWN, 2)
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.EAST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.NORTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.WEST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.WEST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.SOUTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.SOUTH)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.EAST)).block))
            result = result.coerceAtMost(getBlockLevel(world.getBlockState(mutable.move(Direction.EAST)).block))

            return result
        }

        fun checkTorchBlockStates(world: World, tablePos: BlockPos): Int {
            var result = 4
            val mutable = tablePos.mutableCopy().move(Direction.UP)

            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.EAST, 2)).block))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.NORTH, 2)).block))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.WEST, 2)).block))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.WEST, 2)).block))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.SOUTH, 2)).block))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.SOUTH, 2)).block))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.EAST, 2)).block))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.EAST, 2)).block))

            return result
        }

        private fun getFluid(world: World, tablePos: BlockPos): Fluid {
            val states = mutableListOf<FluidState>()
            val mutable = tablePos.mutableCopy().move(Direction.DOWN)

            states.add(world.getFluidState(mutable.move(Direction.EAST)))
            states.add(world.getFluidState(mutable.move(Direction.NORTH)))
            states.add(world.getFluidState(mutable.move(Direction.WEST)))
            states.add(world.getFluidState(mutable.move(Direction.WEST)))
            states.add(world.getFluidState(mutable.move(Direction.SOUTH)))
            states.add(world.getFluidState(mutable.move(Direction.SOUTH)))
            states.add(world.getFluidState(mutable.move(Direction.EAST)))
            states.add(world.getFluidState(mutable.move(Direction.EAST)))

            if (states.any { !it.isStill })
                return Fluids.EMPTY

            val result = states.asSequence()
                .map { it.fluid }
                .distinct()

            if (result.count() == 1) {
                return result.first()
            }
            return Fluids.EMPTY
        }

        fun getInventory(world: World, pos: BlockPos, player: PlayerEntity, level: Int): RitualInventory {
            val itemEntities = world.getEntitiesByType(EntityType.ITEM, Box(pos).expand(2.0, 1.0, 2.0)) { true }
            val fluid = getFluid(world, pos)

            return RitualInventory(itemEntities, fluid, pos, player, level)
        }

        private fun getBlockLevel(block: Block): Int {
            return when {
                level1RitualMaterialsTag.contains(block) -> 1
                level0RitualMaterialsTag.contains(block) -> 0
                else -> -1
            }
        }

        private fun getTorchLevel(block: Block): Int {
            return when {
                level1RitualTorchesTag.contains(block) -> 1
                level0RitualTorchesTag.contains(block) -> 0
                else -> -1
            }
        }
    }
}