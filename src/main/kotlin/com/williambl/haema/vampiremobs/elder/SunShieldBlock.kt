package com.williambl.haema.vampiremobs.elder

import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.world.ServerWorld
import net.minecraft.state.StateManager
import net.minecraft.state.property.IntProperty
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Direction
import net.minecraft.util.math.random.Random
import net.minecraft.world.World

class SunShieldBlock(settings: Settings) : Block(settings) {
    init {
        defaultState = defaultState.with(DISTANCE, 0)
    }

    @Deprecated(message = "do not call directly")
    override fun onBlockAdded(
        state: BlockState,
        world: World,
        pos: BlockPos,
        oldState: BlockState,
        notify: Boolean
    ) {
        super.onBlockAdded(state, world, pos, oldState, notify)
        if (state.get(DISTANCE) < 10) {
            world.createAndScheduleBlockTick(pos, this, 2)
        }
    }

    @Deprecated(message = "do not call directly")
    override fun scheduledTick(state: BlockState, world: ServerWorld, pos: BlockPos, random: Random) {
        val distance = state.get(DISTANCE) + 1
        if (distance > 10) {
            return
        }
        Direction.Type.HORIZONTAL.stream().forEach { dir ->
            val offsetPos = pos.offset(dir)
            if (world.isAir(offsetPos) || world.getBlockState(offsetPos).material.isReplaceable) {
                world.setBlockState(offsetPos, state.with(DISTANCE, distance))
                world.spawnParticles(DustParticleEffect(DustParticleEffect.RED, 2f), pos.x + 0.5, pos.y + 0.5, pos.z + 0.5, 4, 0.5, 0.5, 0.5, 0.0)
            }
        }
    }

    @Deprecated(message = "do not call directly")
    override fun randomTick(state: BlockState, world: ServerWorld, pos: BlockPos, random: Random) {
        if (random.nextInt(10) == 0) {
            world.breakBlock(pos, false)
        }
    }

    override fun appendProperties(builder: StateManager.Builder<Block, BlockState>) {
        super.appendProperties(builder)
        builder.add(DISTANCE)
    }

    companion object {
        val DISTANCE = IntProperty.of("distance", 0, 10)
    }
}