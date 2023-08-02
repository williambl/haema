package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.List;

public class DrinkFromAttackTarget<E extends Mob> extends DrinkFromTarget<E> {
    private static final List<Pair<MemoryModuleType<?>, MemoryStatus>> MEMORY_REQUIREMENTS = ObjectArrayList.of(
            Pair.of(MemoryModuleType.ATTACK_TARGET, MemoryStatus.VALUE_PRESENT));


    public DrinkFromAttackTarget(int delayTicks) {
        super(delayTicks);
    }

    @Override
    protected LivingEntity getTarget(ServerLevel level, E entity) {
        return BrainUtils.getTargetOfEntity(entity);
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> createMemoryRequirements() {
        return MEMORY_REQUIREMENTS;
    }
}
