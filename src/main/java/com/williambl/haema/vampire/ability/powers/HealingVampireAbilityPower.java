package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

public record HealingVampireAbilityPower(VExpression predicate, VExpression amountToHeal, VExpression bloodUsage) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<HealingVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).fieldOf("predicate").forGetter(HealingVampireAbilityPower::predicate),
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY).optionalFieldOf("amount_to_heal", VExpression.value(StandardVTypes.NUMBER, (double) 1.0)).forGetter(HealingVampireAbilityPower::amountToHeal),
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY).optionalFieldOf("blood_usage", VExpression.value(StandardVTypes.NUMBER, (double) 1.0)).forGetter(HealingVampireAbilityPower::bloodUsage)
    ).apply(instance, HealingVampireAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
        if (entity.level().isClientSide()) {
            return;
        }

        if (!DFunctions.<Boolean>evaluate(this.predicate(), DFunctions.createEntityContext(entity))) {
            return;
        }

        this.heal(entity);
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    private void heal(LivingEntity entity) {
        var vampireComponent = entity.getComponent(VampireComponent.KEY);
        vampireComponent.setBlood(vampireComponent.getBlood() - DFunctions.<Double>evaluate(this.bloodUsage(), DFunctions.createEntityContext(entity)));
        entity.heal(DFunctions.<Double>evaluate(this.amountToHeal(), DFunctions.createEntityContext(entity)).floatValue());
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }
}
