package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.HaemaVampires;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.behavior.BehaviorUtils;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.tslat.smartbrainlib.api.core.behaviour.DelayedBehaviour;
import net.tslat.smartbrainlib.api.core.behaviour.custom.attack.AnimatableMeleeAttack;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.ArrayList;
import java.util.List;

public class DrinkFromDrinkTarget<E extends Mob> extends DrinkFromTarget<E> {
    private static final List<Pair<MemoryModuleType<?>, MemoryStatus>> MEMORY_REQUIREMENTS = ObjectArrayList.of(
            Pair.of(HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET, MemoryStatus.VALUE_PRESENT));


    public DrinkFromDrinkTarget(int delayTicks) {
        super(delayTicks);
    }

    @Override
    protected LivingEntity getTarget(ServerLevel level, E entity) {
        return BrainUtils.getMemory(entity, HaemaVampireMobs.VampireMobMemoryModuleTypes.BLOOD_DRINKING_TARGET);
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> createMemoryRequirements() {
        return MEMORY_REQUIREMENTS;
    }
}
