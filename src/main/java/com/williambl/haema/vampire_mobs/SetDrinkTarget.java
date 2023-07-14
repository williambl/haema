package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import com.williambl.haema.api.content.blood.BloodApi;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.entity.ai.memory.NearestVisibleLivingEntities;
import net.minecraft.world.entity.player.Player;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.List;
import java.util.function.Predicate;

public class SetDrinkTarget<E extends Mob> extends ExtendedBehaviour<E> {
    private static final List<Pair<MemoryModuleType<?>, MemoryStatus>> MEMORY_REQUIREMENTS
            = ObjectArrayList.of(Pair.of(HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET, MemoryStatus.VALUE_ABSENT));

    protected Predicate<LivingEntity> canAttackPredicate = entity -> entity.isAlive() && BloodApi.getBloodQuality(entity).isPresent() && (!(entity instanceof Player player) || !player.isCreative());
    private LivingEntity toTarget = null;

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return MEMORY_REQUIREMENTS;
    }

    @Override
    protected boolean checkExtraStartConditions(ServerLevel level, E entity) {
        var nearby = BrainUtils.getMemory(entity, MemoryModuleType.NEAREST_VISIBLE_LIVING_ENTITIES);
        if (nearby != null) {
            this.toTarget = nearby.findClosest(this.canAttackPredicate).orElse(null);
            return this.toTarget != null;
        }

        return false;
    }

    @Override
    protected void start(E entity) {
        BrainUtils.setMemory(entity, HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET, this.toTarget);
        BrainUtils.clearMemory(entity, MemoryModuleType.CANT_REACH_WALK_TARGET_SINCE);

        this.toTarget = null;
    }
}
