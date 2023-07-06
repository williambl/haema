package com.williambl.haema.hunters;

import com.mojang.datafixers.util.Pair;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.Brain;
import net.minecraft.world.entity.ai.behavior.EntityTracker;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.entity.ai.memory.WalkTarget;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

public class GoToLeaderIfFar<E extends LivingEntity> extends ExtendedBehaviour<E> {
    private int maxDistanceFromLeaderSqr = 20 * 20;
    private Entity target;
    private float speedModifier = 1f;
    private int closeEnoughDistance = 10;

    public GoToLeaderIfFar() {
    }

    public GoToLeaderIfFar<E> maxDistance(int dist) {
        this.maxDistanceFromLeaderSqr = dist * dist;
        return this;
    }

    public GoToLeaderIfFar<E> closeEnoughDistance(int closeEnoughDistance) {
        this.closeEnoughDistance = closeEnoughDistance;
        return this;
    }

    public GoToLeaderIfFar<E> speedModifier(float speedModifier) {
        this.speedModifier = speedModifier;
        return this;
    }

    @Override
    protected boolean checkExtraStartConditions(ServerLevel level, E entity) {
        var leaderUuid = BrainUtils.getMemory(entity, HaemaHunters.HunterMemoryModuleTypes.LEADER);
        if (leaderUuid == null) {
            return false;
        }
        this.target = level.getEntity(leaderUuid);
        if (this.target != null && this.target.distanceToSqr(entity) > this.maxDistanceFromLeaderSqr) {
            this.target = null;
        }
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
                Pair.of(HaemaHunters.HunterMemoryModuleTypes.LEADER, MemoryStatus.VALUE_PRESENT));
    }
}
