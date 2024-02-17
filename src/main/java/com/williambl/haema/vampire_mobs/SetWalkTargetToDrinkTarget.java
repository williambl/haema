package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.Brain;
import net.minecraft.world.entity.ai.behavior.BehaviorUtils;
import net.minecraft.world.entity.ai.behavior.EntityTracker;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.entity.ai.memory.WalkTarget;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.List;
import java.util.Objects;

/**
 * @see net.tslat.smartbrainlib.api.core.behaviour.custom.path.SetWalkTargetToAttackTarget
 */
public class SetWalkTargetToDrinkTarget<E extends Mob> extends ExtendedBehaviour<E> {
    private static final List<Pair<MemoryModuleType<?>, MemoryStatus>> MEMORY_REQUIREMENTS = ObjectArrayList.of(Pair.of(MemoryModuleType.WALK_TARGET, MemoryStatus.REGISTERED), Pair.of(MemoryModuleType.LOOK_TARGET, MemoryStatus.REGISTERED), Pair.of(HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET, MemoryStatus.VALUE_PRESENT));

    protected float speedModifier = 1;

    /**
     * Set the movespeed modifier for the entity when moving to the target.
     * @param speedModifier The movespeed modifier/multiplier
     * @return this
     */
    public SetWalkTargetToDrinkTarget<E> speedMod(float speedModifier) {
        this.speedModifier = speedModifier;

        return this;
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return MEMORY_REQUIREMENTS;
    }

    @Override
    protected void start(E entity) {
        Brain<?> brain = entity.getBrain();
        LivingEntity target = Objects.requireNonNull(BrainUtils.getMemory(entity, HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET));

        if (entity.getSensing().hasLineOfSight(target) && BehaviorUtils.isWithinAttackRange(entity, target, 1)) {
            BrainUtils.clearMemory(brain, MemoryModuleType.WALK_TARGET);
        } else {
            BrainUtils.setMemory(brain, MemoryModuleType.LOOK_TARGET, new EntityTracker(target, true));
            BrainUtils.setMemory(brain, MemoryModuleType.WALK_TARGET, new WalkTarget(new EntityTracker(target, false), this.speedModifier, 0));
        }
    }
}
