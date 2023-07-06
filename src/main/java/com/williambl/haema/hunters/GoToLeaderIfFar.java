package com.williambl.haema.hunters;

import com.mojang.datafixers.util.Pair;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.Brain;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

public class GoToLeaderIfFar<E extends LivingEntity> extends InteractWithOthers<E> {
    private int maxDistanceFromLeaderSqr = 20 * 20;

    public GoToLeaderIfFar(E entity) {
        this.interactIf(possibleLeader ->
                possibleLeader.getUUID().equals(BrainUtils.getMemory(entity, HaemaHunters.HunterMemoryModuleTypes.LEADER))
                        && possibleLeader.distanceToSqr(entity) >= this.maxDistanceFromLeaderSqr);
    }

    public GoToLeaderIfFar<E> maxDistance(int dist) {
        this.maxDistanceFromLeaderSqr = dist * dist;
        return this;
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        var reqs = new ArrayList<>(super.getMemoryRequirements());
        reqs.add(Pair.of(HaemaHunters.HunterMemoryModuleTypes.LEADER, MemoryStatus.VALUE_PRESENT));
        return reqs;
    }
}
