package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.Codec;
import com.williambl.haema.vampire.ability.VampireAbility;
import com.williambl.haema.vampire.ability.VampireAbilityPower;
import io.github.apace100.apoli.component.PowerHolderComponent;
import io.github.apace100.apoli.power.PowerTypeRegistry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

public record DummyVampireAbilityPower(ResourceLocation powerName) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<DummyVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(ResourceLocation.CODEC.fieldOf("id")
            .xmap(DummyVampireAbilityPower::new, DummyVampireAbilityPower::powerName));

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
    public KeyDispatchDataCodec<DummyVampireAbilityPower> codec() {
        return CODEC;
    }
}
