package com.williambl.haema.hunters;

import com.mojang.datafixers.util.Pair;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.item.CrossbowItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.List;
import java.util.function.*;

public class SwitchWeapons<E extends LivingEntity> extends ExtendedBehaviour<E> {
    public SwitchWeapons(BiFunction<E, LivingEntity, Predicate<ItemStack>> weaponForTarget, Consumer<Predicate<ItemStack>> switchToWeapon, Predicate<Predicate<ItemStack>> canUseWeapon) {
        this.startCondition(e -> {
            var target = BrainUtils.getTargetOfEntity(e);
            var wantedWeapon = weaponForTarget.apply(e, target);
            if (e.isHolding(wantedWeapon)) {
                return false;
            }
            return canUseWeapon.test(wantedWeapon);
        });
        this.whenStarting(e -> {
            var target = BrainUtils.getTargetOfEntity(e);
            var wantedWeapon = weaponForTarget.apply(e, target);
            switchToWeapon.accept(wantedWeapon);
        });
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return List.of(Pair.of(MemoryModuleType.ATTACK_TARGET, MemoryStatus.VALUE_PRESENT));
    }
}
