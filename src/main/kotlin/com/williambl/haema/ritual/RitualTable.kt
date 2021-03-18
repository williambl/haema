package com.williambl.haema.ritual

import com.williambl.haema.Vampirable
import com.williambl.haema.ritual.craft.RitualInventory
import com.williambl.haema.ritual.craft.RitualRecipe
import com.williambl.haema.util.MultiTagMatcher
import net.fabricmc.fabric.api.tag.TagRegistry
import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.block.Material
import net.minecraft.block.ShapeContext
import net.minecraft.entity.Entity
import net.minecraft.entity.EntityType
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.fluid.Fluid
import net.minecraft.fluid.FluidState
import net.minecraft.fluid.Fluids
import net.minecraft.item.BlockItem
import net.minecraft.item.Item
import net.minecraft.item.ItemGroup
import net.minecraft.particle.DustParticleEffect
import net.minecraft.particle.ParticleTypes
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.state.property.Properties
import net.minecraft.tag.Tag
import net.minecraft.util.ActionResult
import net.minecraft.util.Hand
import net.minecraft.util.Identifier
import net.minecraft.util.hit.BlockHitResult
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Box
import net.minecraft.util.math.Direction
import net.minecraft.util.registry.Registry
import net.minecraft.util.shape.VoxelShape
import net.minecraft.world.BlockView
import net.minecraft.world.World
import vazkii.patchouli.common.multiblock.DenseMultiblock
import vazkii.patchouli.common.multiblock.MultiblockRegistry
import vazkii.patchouli.common.multiblock.StateMatcher
import java.util.*
import kotlin.math.min

val level0RitualMaterialsTag: Tag<Block> = TagRegistry.block(Identifier("haema:ritual_materials/level_0"))
val level1RitualMaterialsTag: Tag<Block> = TagRegistry.block(Identifier("haema:ritual_materials/level_1"))

val level0RitualTorchesTag: Tag<Block> = TagRegistry.block(Identifier("haema:ritual_torches/level_0"))
val level1RitualTorchesTag: Tag<Block> = TagRegistry.block(Identifier("haema:ritual_torches/level_1"))

class RitualTable(settings: Settings) : Block(settings) {
    override fun onUse(
        state: BlockState,
        world: World,
        pos: BlockPos,
        player: PlayerEntity,
        hand: Hand,
        hit: BlockHitResult
    ): ActionResult {
        if (!world.isClient) {
            if (pos != player.blockPos) return ActionResult.PASS

            val level = min(checkBaseBlockStates(world, pos), checkTorchBlockStates(world, pos))

            val inventory = getInventory(world, pos, player, level)

            (world as ServerWorld).server.recipeManager.listAllOfType(RitualRecipe.recipeType)
                .firstOrNull { it.matches(inventory) }
                ?.craft(inventory) ?: return ActionResult.PASS
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
                    spawnParticles(world, pos, world.random.nextDouble() * 0.4, showExtras, level)
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
                spawnParticles(world, pos, random.nextDouble() * 0.2, showExtras, level)
            }
        }
        super.randomDisplayTick(state, world, pos, random)
    }

    private fun spawnParticles(world: World, pos: BlockPos, speed: Double, showExtras: Boolean, level: Int) {
        world.addParticle(if (level > 0) ParticleTypes.SOUL_FIRE_FLAME else ParticleTypes.FLAME, pos.x + 2.5, pos.y + 1.5, pos.z + 0.5, -speed, 0.0, 0.0)
        world.addParticle(if (level > 0) ParticleTypes.SOUL_FIRE_FLAME else ParticleTypes.FLAME, pos.x + 2.5, pos.y + 1.5, pos.z + 2.5, -speed, 0.0, -speed)
        world.addParticle(if (level > 0) ParticleTypes.SOUL_FIRE_FLAME else ParticleTypes.FLAME, pos.x + 2.5, pos.y + 1.5, pos.z - 1.5, -speed, 0.0, speed)
        world.addParticle(if (level > 0) ParticleTypes.SOUL_FIRE_FLAME else ParticleTypes.FLAME, pos.x + 0.5, pos.y + 1.5, pos.z + 2.5, 0.0, 0.0, -speed)
        world.addParticle(if (level > 0) ParticleTypes.SOUL_FIRE_FLAME else ParticleTypes.FLAME, pos.x + 0.5, pos.y + 1.5, pos.z - 1.5, 0.0, 0.0, speed)
        world.addParticle(if (level > 0) ParticleTypes.SOUL_FIRE_FLAME else ParticleTypes.FLAME, pos.x - 1.5, pos.y + 1.5, pos.z + 0.5, speed, 0.0, 0.0)
        world.addParticle(if (level > 0) ParticleTypes.SOUL_FIRE_FLAME else ParticleTypes.FLAME, pos.x - 1.5, pos.y + 1.5, pos.z + 2.5, speed, 0.0, -speed)
        world.addParticle(if (level > 0) ParticleTypes.SOUL_FIRE_FLAME else ParticleTypes.FLAME, pos.x - 1.5, pos.y + 1.5, pos.z - 1.5, speed, 0.0, speed)

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
        val instance: RitualTable by lazy { RitualTable(Settings.of(Material.METAL)) }

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

            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.EAST, 2))))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.NORTH, 2))))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.WEST, 2))))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.WEST, 2))))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.SOUTH, 2))))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.SOUTH, 2))))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.EAST, 2))))
            result = result.coerceAtMost(getTorchLevel(world.getBlockState(mutable.move(Direction.EAST, 2))))

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

        private fun getTorchLevel(blockState: BlockState): Int {
            return when {
                blockState.contains(Properties.LIT) && !blockState[Properties.LIT] -> -1
                blockState.isIn(level1RitualTorchesTag) -> 1
                blockState.isIn(level0RitualTorchesTag) -> 0
                else -> -1
            }
        }
    }
}

fun registerRitualTable() {
    Registry.register(
        Registry.BLOCK,
        Identifier("haema:ritual_table"),
        RitualTable.instance
    )
    Registry.register(
        Registry.ITEM,
        Identifier("haema:ritual_table"),
        BlockItem(RitualTable.instance, Item.Settings().group(ItemGroup.DECORATIONS))
    )

    //To make them load and register
    RitualRecipe.recipeSerializer
    RitualRecipe.recipeType

    MultiblockRegistry.registerMultiblock(Identifier("haema:basic_altar"), DenseMultiblock(
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
            'T' to MultiTagMatcher(listOf(level0RitualTorchesTag as Tag.Identified<Block>), mapOf(Properties.LIT to true)),
            'B' to MultiTagMatcher(listOf(level0RitualMaterialsTag as Tag.Identified<Block>), mapOf()),
            '0' to StateMatcher.fromBlockLoose(RitualTable.instance),
            ' ' to StateMatcher.ANY
        )
    ))

    MultiblockRegistry.registerMultiblock(Identifier("haema:blackstone_altar"), DenseMultiblock(
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
            'T' to MultiTagMatcher(listOf(level1RitualTorchesTag as Tag.Identified<Block>), mapOf(Properties.LIT to true)),
            'B' to MultiTagMatcher(listOf(level1RitualMaterialsTag as Tag.Identified<Block>), mapOf()),
            '0' to StateMatcher.fromBlockLoose(RitualTable.instance),
            ' ' to StateMatcher.ANY
        )
    ))
}