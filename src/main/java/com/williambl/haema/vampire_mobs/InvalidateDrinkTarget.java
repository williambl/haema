package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.entity.player.Player;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;
import org.apache.commons.lang3.function.ToBooleanBiFunction;

import java.util.List;

/**
 * @see net.tslat.smartbrainlib.api.core.behaviour.custom.target.InvalidateAttackTarget
 */
public class InvalidateDrinkTarget<E extends LivingEntity> extends ExtendedBehaviour<E> {
    private static final List<Pair<MemoryModuleType<?>, MemoryStatus>> MEMORY_REQUIREMENTS = ObjectArrayList.of(Pair.of(HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET, MemoryStatus.VALUE_PRESENT), Pair.of(MemoryModuleType.CANT_REACH_WALK_TARGET_SINCE, MemoryStatus.REGISTERED));

    protected ToBooleanBiFunction<E, LivingEntity> customPredicate = (entity, target) -> target instanceof Player pl && (pl.isCreative() || pl.isSpectator());
    protected long pathfindingAttentionSpan = 200;

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return MEMORY_REQUIREMENTS;
    }

    /**
     * Sets a custom predicate to invalidate the drinking target if none of the previous checks invalidate it first.<br>
     * Overrides the default player gamemode check
     */
    public InvalidateDrinkTarget<E> invalidateIf(ToBooleanBiFunction<E, LivingEntity> predicate) {
        this.customPredicate = predicate;

        return this;
    }

    /**
     * Skips the check to see if the entity has been unable to path to its target for a while
     */
    public InvalidateDrinkTarget<E> ignoreFailedPathfinding() {
        return this.stopTryingToPathAfter(0);
    }

    /**
     * Sets the attention span for the brain owner's pathfinding. If the entity has been unable to find a good path to
     * the target after this time, it will invalidate the target.
     */
    public InvalidateDrinkTarget<E> stopTryingToPathAfter(long ticks) {
        this.pathfindingAttentionSpan = ticks;

        return this;
    }

    @Override
    protected void start(E entity) {
        LivingEntity target = BrainUtils.getMemory(entity, HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET);

        if (target == null) {
            return;
        }

        if (this.isTargetInvalid(entity, target) || !this.canAttack(entity, target) ||
                this.isTiredOfPathing(entity) || this.customPredicate.applyAsBoolean(entity, target)) {
            BrainUtils.clearMemory(entity, HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET);
        }
    }

    protected boolean isTargetInvalid(E entity, LivingEntity target) {
        if (entity.level != target.level) {
            return true;
        }

        return target.isDeadOrDying() || target.isRemoved();
    }

    protected boolean canAttack(E entity, LivingEntity target) {
        return entity.canAttack(target);
    }

    protected boolean isTiredOfPathing(E entity) {
        if (this.pathfindingAttentionSpan <= 0) {
            return false;
        }

        Long time = BrainUtils.getMemory(entity, MemoryModuleType.CANT_REACH_WALK_TARGET_SINCE);

        return time != null && entity.level.getGameTime() - time > this.pathfindingAttentionSpan;
    }
}
