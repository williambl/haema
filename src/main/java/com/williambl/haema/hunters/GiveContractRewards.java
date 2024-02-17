package com.williambl.haema.hunters;

import com.mojang.datafixers.util.Pair;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.storage.loot.LootParams;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.minecraft.world.level.storage.loot.parameters.LootContextParams;
import net.tslat.smartbrainlib.api.core.behaviour.DelayedBehaviour;

import java.util.List;

public class GiveContractRewards<E extends LivingEntity> extends DelayedBehaviour<E> {
    public GiveContractRewards(int delayTicks, ResourceLocation lootTable) {
        super(delayTicks);
        this.startCondition(this::isHoldingFulfilledContract);
        this.whenActivating(e -> {
            var handHoldingContract = ProjectileUtil.getWeaponHoldingHand(e, HaemaHunters.HunterItems.VAMPIRE_HUNTER_CONTRACT);
            e.setItemInHand(handHoldingContract, ItemStack.EMPTY);
            var server = e.level().getServer();
            if (server == null) {
                return;
            }

            server.getLootData().getLootTable(lootTable).getRandomItems(
                new LootParams.Builder((ServerLevel) e.level())
                        .withParameter(LootContextParams.THIS_ENTITY, e).create(LootContextParamSets.PIGLIN_BARTER))
                    .forEach(e::spawnAtLocation);
        });
        this.stopIf(e -> !this.isHoldingFulfilledContract(e));
    }

    private boolean isHoldingFulfilledContract(E mob) {
        return this.isFulfilledContract(mob.getMainHandItem()) || this.isFulfilledContract(mob.getOffhandItem());
    }

    private boolean isFulfilledContract(ItemStack stack) {
        return stack.is(HaemaHunters.HunterItems.VAMPIRE_HUNTER_CONTRACT) && VampireHunterContractItem.isFulfilled(stack);
    }

    @Override
    protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
        return List.of(Pair.of(MemoryModuleType.ATTACK_TARGET, MemoryStatus.VALUE_ABSENT));
    }
}
