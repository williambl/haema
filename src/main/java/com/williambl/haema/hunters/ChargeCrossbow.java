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

import java.util.List;
import java.util.function.BiConsumer;

public class ChargeCrossbow<E extends LivingEntity> extends ExtendedBehaviour<E> {
    public ChargeCrossbow(BiConsumer<E, Boolean> setCharging) {
        this.startCondition(e -> {
            var stack = this.getCrossbowStack(e);
            return stack.getItem() instanceof CrossbowItem && !CrossbowItem.isCharged(stack);
        });
        this.whenStarting(e -> {
            e.startUsingItem(ProjectileUtil.getWeaponHoldingHand(e, Items.CROSSBOW));
            setCharging.accept(e, true);
        });
        this.whenStopping(e -> {
            e.releaseUsingItem();
            setCharging.accept(e, false);
        });
        this.stopIf(e -> e.getTicksUsingItem() >= this.getCrossbowStack(e).getUseDuration());
    }

    private ItemStack getCrossbowStack(E entity) {
        return entity.getItemInHand(ProjectileUtil.getWeaponHoldingHand(entity, Items.CROSSBOW));
    }

    @Override
    protected boolean shouldKeepRunning(E entity) {
        return this.getCrossbowStack(entity).getItem() instanceof CrossbowItem;
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return List.of();
    }
}
