package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.DFunction;
import com.williambl.dfunc.number.EntityNumberDFunctions;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

public record HealingVampireAbilityPower(DFunction<Entity, Boolean> predicate, DFunction<Entity, Double> amountToHeal, DFunction<Entity, Double> bloodUsage) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<HealingVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunction.ENTITY_PREDICATE_TYPE_REGISTRY.codec().fieldOf("predicate").forGetter(HealingVampireAbilityPower::predicate),
            DFunction.ENTITY_TO_NUMBER_FUNCTION_TYPE_REGISTRY.codec().optionalFieldOf("amount_to_heal", EntityNumberDFunctions.CONSTANT.factory().apply(1.0)).forGetter(HealingVampireAbilityPower::amountToHeal),
            DFunction.ENTITY_TO_NUMBER_FUNCTION_TYPE_REGISTRY.codec().optionalFieldOf("blood_used", EntityNumberDFunctions.CONSTANT.factory().apply(1.0)).forGetter(HealingVampireAbilityPower::bloodUsage)
    ).apply(instance, HealingVampireAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
        if (!this.predicate().apply(entity)) {
            return;
        }

        this.heal(entity);
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    private void heal(LivingEntity entity) {
        var vampireComponent = entity.getComponent(VampireComponent.KEY);
        vampireComponent.setBlood(vampireComponent.getBlood() - this.bloodUsage().apply(entity));
        entity.heal(this.amountToHeal().apply(entity).floatValue());
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }
}
