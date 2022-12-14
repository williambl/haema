package com.williambl.haema.vampiremobs.elder

import com.mojang.datafixers.util.Pair
import com.williambl.haema.ability.component.dash.DashAbilityComponent.Companion.entityKey
import net.minecraft.entity.ai.brain.MemoryModuleState
import net.minecraft.entity.ai.brain.MemoryModuleType
import net.minecraft.entity.ai.brain.WalkTarget
import net.minecraft.entity.mob.PathAwareEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Vec3d
import net.tslat.smartbrainlib.api.core.behaviour.DelayedBehaviour
import net.tslat.smartbrainlib.api.util.BrainUtils

class DashToWalkTarget<E : PathAwareEntity>(val dashTargetFunc: () -> Vec3d?) : DelayedBehaviour<E>(10) {
    protected var lastTargetPos: BlockPos? = null

    override fun getMemoryRequirements(): List<Pair<MemoryModuleType<*>, MemoryModuleState>> {
        return MEMORY_REQUIREMENTS
    }

    override fun shouldRun(level: ServerWorld, entity: E): Boolean {
        if (!entity.getComponent(entityKey).canDash() || this.dashTargetFunc() == null) {
            return false
        }

        val brain = entity.brain
        val walkTarget = BrainUtils.getMemory(brain, MemoryModuleType.WALK_TARGET) ?: return false
        return if (!hasReachedTarget(entity, walkTarget)) {
            lastTargetPos = walkTarget.lookTarget.blockPos
            true
        } else {
            BrainUtils.clearMemory(brain, MemoryModuleType.WALK_TARGET)
            BrainUtils.clearMemory(brain, MemoryModuleType.CANT_REACH_WALK_TARGET_SINCE)
            false
        }
    }

    override fun doDelayedAction(entity: E) {
        entity.getComponent(entityKey).dash()
    }

    protected fun hasReachedTarget(entity: E, target: WalkTarget): Boolean {
        return target.lookTarget.blockPos.getManhattanDistance(entity.blockPos) <= target.completionRange
    }

    companion object {
        private val MEMORY_REQUIREMENTS: List<Pair<MemoryModuleType<*>, MemoryModuleState>> = listOf(
            Pair.of(
                MemoryModuleType.CANT_REACH_WALK_TARGET_SINCE,
                MemoryModuleState.REGISTERED
            ),
            Pair.of(
                MemoryModuleType.PATH,
                MemoryModuleState.VALUE_ABSENT
            ),
            Pair.of(
                MemoryModuleType.WALK_TARGET,
                MemoryModuleState.VALUE_PRESENT
            )
        )
    }
}