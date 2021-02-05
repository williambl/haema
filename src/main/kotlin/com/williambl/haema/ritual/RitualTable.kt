package com.williambl.haema.ritual

import com.williambl.haema.level0RitualMaterialsTag
import com.williambl.haema.level0RitualTorchesTag
import com.williambl.haema.level1RitualMaterialsTag
import com.williambl.haema.level1RitualTorchesTag
import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.util.ActionResult
import net.minecraft.util.Hand
import net.minecraft.util.hit.BlockHitResult
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Direction
import net.minecraft.world.World

class RitualTable(settings: Settings) : Block(settings) {

    override fun onUse(
        state: BlockState,
        world: World,
        pos: BlockPos,
        player: PlayerEntity?,
        hand: Hand,
        hit: BlockHitResult
    ): ActionResult {
        println("base block level: ${checkBaseBlockStates(world, pos)}")
        println("torch level: ${checkTorchBlockStates(world, pos)}")

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