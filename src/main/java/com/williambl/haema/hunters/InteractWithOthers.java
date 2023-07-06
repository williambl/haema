package com.williambl.haema.hunters;

import com.mojang.datafixers.util.Pair;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.behavior.EntityTracker;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.entity.ai.memory.WalkTarget;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.function.Predicate;

public class InteractWithOthers<E extends LivingEntity> extends ExtendedBehaviour<E> {
    private Predicate<LivingEntity> interactWithPredicate;
    private float speedModifier = 1f;
    private int closeEnoughDistance = 2;
    private LivingEntity target = null;

    public InteractWithOthers(Predicate<LivingEntity> interactWithPredicate) {
        this.interactWithPredicate = interactWithPredicate;
    }

    public InteractWithOthers() {
        this.interactWithPredicate = $ -> true;
    }

    public InteractWithOthers<E> interactIf(Predicate<LivingEntity> predicate) {
        this.interactWithPredicate = predicate;
        return this;
    }

    public InteractWithOthers<E> closeEnoughDistance(int closeEnoughDistance) {
        this.closeEnoughDistance = closeEnoughDistance;
        return this;
    }

    public InteractWithOthers<E> speedModifier(float speedModifier) {
        this.speedModifier = speedModifier;
        return this;
    }

    @Override
    protected boolean checkExtraStartConditions(ServerLevel level, E entity) {
        var targets = BrainUtils.getMemory(entity, MemoryModuleType.NEAREST_VISIBLE_LIVING_ENTITIES)
                .find(this.interactWithPredicate).toList();
        this.target = targets.isEmpty() ? null : targets.get(entity.getRandom().nextInt(targets.size()));
        return this.target != null;
    }

    @Override
    protected void start(E entity) {
        if (this.target != null) {
            BrainUtils.setMemory(entity, MemoryModuleType.LOOK_TARGET, new EntityTracker(this.target, true));
            BrainUtils.setMemory(entity, MemoryModuleType.WALK_TARGET, new WalkTarget(new EntityTracker(this.target, false), this.speedModifier, this.closeEnoughDistance));
        }
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return List.of(
                Pair.of(MemoryModuleType.LOOK_TARGET, MemoryStatus.REGISTERED),
                Pair.of(MemoryModuleType.WALK_TARGET, MemoryStatus.VALUE_ABSENT),
                Pair.of(MemoryModuleType.NEAREST_LIVING_ENTITIES, MemoryStatus.VALUE_PRESENT));
    }
}
