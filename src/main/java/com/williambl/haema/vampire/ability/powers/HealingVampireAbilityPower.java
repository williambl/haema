package com.williambl.haema.vampire.ability.powers;

import com.williambl.dpred.DPredicate;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

public record HealingVampireAbilityPower(DPredicate<Entity> predicate) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<HealingVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(
            DPredicate.ENTITY_PREDICATE_TYPE_REGISTRY.codec().fieldOf("predicate").codec().xmap(HealingVampireAbilityPower::new, HealingVampireAbilityPower::predicate));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
        if (!this.predicate().test(entity)) {
            return;
        }

        //TODO actually heal
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }
}
