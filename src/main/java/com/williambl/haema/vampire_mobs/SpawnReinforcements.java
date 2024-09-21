package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.reinforcements.SpawnReinforcementsAbilityPower;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.tslat.smartbrainlib.api.core.behaviour.DelayedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.ToIntFunction;

public class SpawnReinforcements<E extends LivingEntity> extends DelayedBehaviour<E> {
    public static final List<Pair<MemoryModuleType<?>, MemoryStatus>> MEMORY_REQUIREMENTS = List.of(Pair.of(MemoryModuleType.ATTACK_TARGET, MemoryStatus.VALUE_PRESENT));
    private List<SpawnReinforcementsAbilityPower> powers;
    private final Predicate<E> canSpawnPredicate;

    public SpawnReinforcements(
            Predicate<E> canSpawnPredicate,
            int delay) {
        super(delay);
        this.canSpawnPredicate = canSpawnPredicate;
    }

    @Override
    protected boolean checkExtraStartConditions(ServerLevel level, E entity) {
        this.powers = VampireAbilitiesComponent.KEY.maybeGet(entity).map(c -> c.getEnabledPowersOfClass(SpawnReinforcementsAbilityPower.class)).orElse(List.of());
        return !this.powers.isEmpty() && this.canSpawnPredicate.test(entity);
    }

    @Override
    protected void doDelayedAction(E entity) {
        for (var power : this.powers) {
            if (power.trySpawnReinforcements(entity, (ServerLevel) entity.level())) {
                return;
            }
        }
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return MEMORY_REQUIREMENTS;
    }
}
