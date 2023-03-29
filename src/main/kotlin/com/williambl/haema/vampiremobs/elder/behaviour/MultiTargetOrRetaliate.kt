package com.williambl.haema.vampiremobs.elder.behaviour


import com.mojang.datafixers.util.Pair
import com.williambl.haema.util.setMultiTarget
import com.williambl.haema.vampiremobs.VampireMobsModule
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.brain.Brain
import net.minecraft.entity.ai.brain.MemoryModuleState
import net.minecraft.entity.ai.brain.MemoryModuleType
import net.minecraft.entity.mob.MobEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.server.world.ServerWorld
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour
import net.tslat.smartbrainlib.util.BrainUtils
import java.util.function.Predicate


/**
 * Sets multiple attack targets for an entity, in priority order.
 * Priorities:
 *
 *  1. The [MemoryModuleType.NEAREST_ATTACKABLE] memory value
 *  1. The [MemoryModuleType.HURT_BY_ENTITY] memory value
 *  1. The applicable entities from the [MemoryModuleType.NEAREST_VISIBLE_LIVING_ENTITIES] memory value, in distance
 *  increasing order.
 *
 * Defaults:
 *
 *  * Targets any live entity, as long as it's not a creative-mode player
 *
 * @param <E> The entity
 */
class MultiTargetOrRetaliate<E : MobEntity> : ExtendedBehaviour<E>() {
    protected var canAttackPredicate =
        Predicate { entity: LivingEntity -> entity.isAlive && (entity !is PlayerEntity || !entity.isCreative) }
    protected var targets: List<LivingEntity> = listOf()
    protected var priorityTargetMemory: MemoryModuleType<out LivingEntity> = MemoryModuleType.NEAREST_ATTACKABLE

    /**
     * Set the predicate to determine whether a given entity should be targeted or not.
     * @param predicate The predicate
     * @return this
     */
    fun attackablePredicate(predicate: Predicate<LivingEntity>): MultiTargetOrRetaliate<E> {
        canAttackPredicate = predicate
        return this
    }

    /**
     * Set the memory type that is checked first to target an entity.
     * Useful for switching to player-only targeting
     * @return this
     */
    fun useMemory(memory: MemoryModuleType<out LivingEntity>): MultiTargetOrRetaliate<E> {
        priorityTargetMemory = memory
        return this
    }

    override fun getMemoryRequirements(): List<Pair<MemoryModuleType<*>, MemoryModuleState>> {
        return MEMORY_REQUIREMENTS
    }

    protected override fun shouldRun(pLevel: ServerWorld?, owner: E): Boolean {
        val brain: Brain<*> = owner.brain
        targets = (
                listOfNotNull(
                    BrainUtils.getMemory(brain, priorityTargetMemory),
                    BrainUtils.getMemory(brain, MemoryModuleType.HURT_BY_ENTITY)
                ).filter { e -> canAttackPredicate.test(e) } + (
                        BrainUtils.getMemory(brain, MemoryModuleType.VISIBLE_MOBS)
                            ?.stream(canAttackPredicate)
                            ?.sorted(Comparator.comparing { e -> e.squaredDistanceTo(owner) })
                            ?.toList() ?: listOf()
                        )
                )
        return targets.isNotEmpty()
    }

    override fun start(entity: E) {
        setMultiTarget(entity, targets)
        BrainUtils.clearMemory(entity, MemoryModuleType.CANT_REACH_WALK_TARGET_SINCE)
        targets = listOf()
    }

    companion object {
        private val MEMORY_REQUIREMENTS: List<Pair<MemoryModuleType<*>, MemoryModuleState>> =
            listOf(
                Pair.of(VampireMobsModule.ATTACK_TARGETS_MEMORY, MemoryModuleState.VALUE_ABSENT),
                Pair.of(MemoryModuleType.HURT_BY, MemoryModuleState.REGISTERED),
                Pair.of(
                    MemoryModuleType.NEAREST_ATTACKABLE,
                    MemoryModuleState.REGISTERED
                ),
                Pair.of(MemoryModuleType.VISIBLE_MOBS, MemoryModuleState.REGISTERED)
            )
    }
}