package com.williambl.haema.vampire.ability.powers.vision;

import com.mojang.serialization.Codec;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

public record VampireVisionVampireAbilityPower() implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<VampireVisionVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(Codec.unit(VampireVisionVampireAbilityPower::new));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public KeyDispatchDataCodec<VampireVisionVampireAbilityPower> codec() {
        return CODEC;
    }
}
