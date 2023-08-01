package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContext;
import com.williambl.dfunc.api.context.DFContextSpec;
import com.williambl.dfunc.api.functions.NumberDFunctions;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

import java.util.function.Function;

public record HealingVampireAbilityPower(DFunction<Boolean> predicate, DFunction<Double> amountToHeal, DFunction<Double> bloodUsage) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<HealingVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY), Function.identity()).fieldOf("predicate").forGetter(HealingVampireAbilityPower::predicate),
            DFunction.NUMBER_FUNCTION.codec().optionalFieldOf("amount_to_heal", NumberDFunctions.CONSTANT.factory().apply(1.0)).forGetter(HealingVampireAbilityPower::amountToHeal),
            DFunction.NUMBER_FUNCTION.codec().optionalFieldOf("blood_usage", NumberDFunctions.CONSTANT.factory().apply(1.0)).forGetter(HealingVampireAbilityPower::bloodUsage)
    ).apply(instance, HealingVampireAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
        if (entity.level().isClientSide()) {
            return;
        }

        if (!this.predicate().apply(DFContext.entity(entity))) {
            return;
        }

        this.heal(entity);
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    private void heal(LivingEntity entity) {
        var vampireComponent = entity.getComponent(VampireComponent.KEY);
        vampireComponent.setBlood(vampireComponent.getBlood() - this.bloodUsage().apply(DFContext.entity(entity)));
        entity.heal(this.amountToHeal().apply(DFContext.entity(entity)).floatValue());
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }
}
