package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dpred.DPredicate;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

import java.util.Arrays;

public record HealingVampireAbilityPower(DPredicate<Entity> predicate, float amountToHeal, double[] bloodUseCoefficients) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<HealingVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DPredicate.ENTITY_PREDICATE_TYPE_REGISTRY.codec().fieldOf("predicate").forGetter(HealingVampireAbilityPower::predicate),
            Codec.FLOAT.optionalFieldOf("amount_to_heal", 1.0f).forGetter(HealingVampireAbilityPower::amountToHeal),
            Codec.DOUBLE.listOf().xmap(l -> l.stream().mapToDouble(d -> d).toArray(), a -> Arrays.stream(a).boxed().toList()).fieldOf("blood_use_coefficients").forGetter(HealingVampireAbilityPower::bloodUseCoefficients)
    ).apply(instance, HealingVampireAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
        if (!this.predicate().test(entity)) {
            return;
        }

        this.heal(entity);
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    private void heal(LivingEntity entity) {
        var vampireComponent = entity.getComponent(VampireComponent.KEY);
        double blood = vampireComponent.getBlood();
        double bloodFactor = blood / VampireComponent.MAX_BLOOD;
        double bloodToRemove = 0.0;
        for (int i = 0; i < this.bloodUseCoefficients().length; i++) {                  //TODO consider making something like DPredicates for double functions, to make this more reusable + flexible
            bloodToRemove += this.bloodUseCoefficients()[i] * Math.pow(bloodFactor, i);
        }
        vampireComponent.setBlood(blood - bloodToRemove);
        entity.heal(this.amountToHeal());
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }
}
