package com.williambl.haema.hunters;

import com.mojang.datafixers.util.Pair;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.item.ItemStack;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class StopHoldingWeapon<E extends LivingEntity> extends ExtendedBehaviour<E> {
    public StopHoldingWeapon(Predicate<ItemStack> isWeapon, Consumer<Predicate<ItemStack>> stopHolding) {
        this.startCondition(e -> e.isHolding(isWeapon));
        this.whenStarting(e -> stopHolding.accept(isWeapon));
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return List.of(Pair.of(MemoryModuleType.ATTACK_TARGET, MemoryStatus.VALUE_ABSENT));
    }
}
