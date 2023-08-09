package com.williambl.haema.vampire_mobs;

import com.mojang.datafixers.util.Pair;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.behavior.BehaviorUtils;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.tslat.smartbrainlib.api.core.behaviour.DelayedBehaviour;

import java.util.List;

public abstract class DrinkFromTarget<E extends Mob> extends DelayedBehaviour<E> {
    private final List<Pair<MemoryModuleType<?>, MemoryStatus>> memoryRequirements;
    private List<DrinkingAbilityPower> drinkingPowers;
    private LivingEntity target;

    public DrinkFromTarget(int delayTicks) {
        super(delayTicks);
        this.memoryRequirements = this.createMemoryRequirements();
    }

    @Override
    protected void start(E entity) {
        entity.swing(InteractionHand.MAIN_HAND);
        BehaviorUtils.lookAtEntity(entity, this.target);
    }

    @Override
    protected boolean checkExtraStartConditions(ServerLevel level, E entity) {
        this.target = this.getTarget(level, entity);

        if (!entity.getSensing().hasLineOfSight(this.target) || !entity.isWithinMeleeAttackRange(this.target)) {
            return false;
        }

        this.drinkingPowers = VampireAbilitiesComponent.KEY.maybeGet(entity).map(c -> c.getPowersOfClass(DrinkingAbilityPower.class)).orElse(List.of());
        return !this.drinkingPowers.isEmpty();
    }

    protected abstract LivingEntity getTarget(ServerLevel level, E entity);
    protected abstract List<Pair<MemoryModuleType<?>, MemoryStatus>> createMemoryRequirements();

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return this.memoryRequirements;
    }

    @Override
    protected void doDelayedAction(E entity) {
        for (var power : this.drinkingPowers) {
            if (power.tryDrink(entity, this.target)) {
                return;
            }
        }
    }
}
