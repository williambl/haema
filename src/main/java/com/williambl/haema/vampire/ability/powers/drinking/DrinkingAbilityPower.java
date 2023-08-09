package com.williambl.haema.vampire.ability.powers.drinking;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.material.Fluids;

import java.util.List;

public record DrinkingAbilityPower(VExpression cooldown, VExpression amountToDrink, VExpression canDrink, List<String> keybinds) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<DrinkingAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY_TARGET).fieldOf("cooldown").forGetter(DrinkingAbilityPower::cooldown),
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY_TARGET).fieldOf("amount_to_drink").forGetter(DrinkingAbilityPower::amountToDrink),
            DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).fieldOf("can_drink").forGetter(DrinkingAbilityPower::canDrink),
            Codec.STRING.listOf().fieldOf("keybinds").forGetter(DrinkingAbilityPower::keybinds)
    ).apply(instance, DrinkingAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    public boolean tryDrink(LivingEntity entity, Entity target) {
        if (!DFunctions.<Boolean>evaluate(this.canDrink(), DFunctions.createEntityTargetContext(entity, target))) {
            return false;
        }

        var bloodQuality = BloodApi.getBloodQuality(target);
        if (bloodQuality.isEmpty()) {
            return false;
        }

        long amountDroplets = DFunctions.<Double>evaluate(this.amountToDrink(), DFunctions.createEntityTargetContext(entity, target)).longValue();
        var bloodFluid = BloodApi.extractBlood(target, amountDroplets);
        if (bloodFluid.isSame(Fluids.EMPTY)) {
            return false;
        }

        var vampireComponent = VampireComponent.KEY.getNullable(entity);
        if (vampireComponent == null) { // should never happen but just in case
            return false;
        }

        vampireComponent.addBlood(BloodApi.dropletsToBloodUnits(amountDroplets));
        return true;
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }
}
