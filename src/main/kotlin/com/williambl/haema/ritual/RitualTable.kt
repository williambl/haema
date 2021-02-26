package com.williambl.haema.ritual

import com.williambl.haema.*
import com.williambl.haema.craft.ritual.RitualInventory
import com.williambl.haema.craft.ritual.RitualRecipe
import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.block.ShapeContext
import net.minecraft.entity.Entity
import net.minecraft.entity.EntityType
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.fluid.Fluid
import net.minecraft.fluid.FluidState
import net.minecraft.fluid.Fluids
import net.minecraft.particle.DustParticleEffect
import net.minecraft.particle.ParticleTypes
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.util.ActionResult
import net.minecraft.util.Hand
import net.minecraft.util.hit.BlockHitResult
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Box
import net.minecraft.util.math.Direction
import net.minecraft.util.shape.VoxelShape
import net.minecraft.world.BlockView
import net.minecraft.world.World
import java.util.*
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

    override fun getOutlineShape(
        state: BlockState?,
        world: BlockView?,
        pos: BlockPos?,
        context: ShapeContext?
    ): VoxelShape = shape

    override fun onSteppedOn(world: World, pos: BlockPos, entity: Entity) {
        if (world.isClient && entity is Vampirable && entity.isVampire && world.random.nextFloat() < 0.1) {
            val level = min(checkBaseBlockStates(world, pos), checkTorchBlockStates(world, pos))

            if (level >= 0) {
                val showExtras = getFluid(world, pos) != Fluids.EMPTY
                for (i in 0..level + 1 + world.random.nextInt(3)) {
                    spawnParticles(world, pos, world.random.nextDouble() * 0.4, showExtras)
                }
                if (world.random.nextDouble() < 0.1) {
                    world.playSound(pos.x+0.5, pos.y+1.0, pos.z+0.5, SoundEvents.BLOCK_BEACON_AMBIENT, SoundCategory.BLOCKS, 1.0f, 1.0f, false)
                }
            }
        }
        super.onSteppedOn(world, pos, entity)
    }

    override fun randomDisplayTick(state: BlockState, world: World, pos: BlockPos, random: Random) {
        val level = min(checkBaseBlockStates(world, pos), checkTorchBlockStates(world, pos))

        if (level >= 0) {
            val showExtras = getFluid(world, pos) != Fluids.EMPTY
            for (i in 0..level + random.nextInt(1)) {
                spawnParticles(world, pos, random.nextDouble() * 0.2, showExtras)
            }
        }
        super.randomDisplayTick(state, world, pos, random)
    }

    private fun spawnParticles(world: World, pos: BlockPos, speed: Double, showExtras: Boolean) {
        world.addParticle(ParticleTypes.SOUL_FIRE_FLAME, pos.x + 2.5, pos.y + 1.5, pos.z + 0.5, -speed, 0.0, 0.0)
        world.addParticle(ParticleTypes.SOUL_FIRE_FLAME, pos.x + 2.5, pos.y + 1.5, pos.z + 2.5, -speed, 0.0, -speed)
        world.addParticle(ParticleTypes.SOUL_FIRE_FLAME, pos.x + 2.5, pos.y + 1.5, pos.z - 1.5, -speed, 0.0, speed)
        world.addParticle(ParticleTypes.SOUL_FIRE_FLAME, pos.x + 0.5, pos.y + 1.5, pos.z + 2.5, 0.0, 0.0, -speed)
        world.addParticle(ParticleTypes.SOUL_FIRE_FLAME, pos.x + 0.5, pos.y + 1.5, pos.z - 1.5, 0.0, 0.0, speed)
        world.addParticle(ParticleTypes.SOUL_FIRE_FLAME, pos.x - 1.5, pos.y + 1.5, pos.z + 0.5, speed, 0.0, 0.0)
        world.addParticle(ParticleTypes.SOUL_FIRE_FLAME, pos.x - 1.5, pos.y + 1.5, pos.z + 2.5, speed, 0.0, -speed)
        world.addParticle(ParticleTypes.SOUL_FIRE_FLAME, pos.x - 1.5, pos.y + 1.5, pos.z - 1.5, speed, 0.0, speed)

        if (showExtras) {
            repeat(20) {
                val offsetX = 3*world.random.nextDouble() - 1.0
                val offsetZ = 3*world.random.nextDouble() - 1.0
                world.addParticle(
                    ParticleTypes.BUBBLE,
                    pos.x + offsetX,
                    pos.y - 0.3,
                    pos.z + offsetZ,
                    0.0,
                    speed * 0.1,
                    0.0
                )
            }
            repeat(40) {
                val offsetX = 3*world.random.nextDouble() - 1.0
                val offsetZ = 3*world.random.nextDouble() - 1.0
                world.addParticle(
                    DustParticleEffect(speed.toFloat(), 0f, 0f, 1f),
                    pos.x + offsetX,
                    pos.y - 0.2,
                    pos.z + offsetZ,
                    0.0,
                    speed,
                    0.0
                )
            }

            world.playSound(3*world.random.nextDouble() - 1.0, pos.y.toDouble(),  3*world.random.nextDouble() - 1.0, SoundEvents.BLOCK_LAVA_POP, SoundCategory.BLOCKS, 1.0f, 1.0f, false)
        }
    }

    companion object {
        val shape: VoxelShape = createCuboidShape(0.0, 0.0, 0.0, 16.0, 12.0, 16.0)

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