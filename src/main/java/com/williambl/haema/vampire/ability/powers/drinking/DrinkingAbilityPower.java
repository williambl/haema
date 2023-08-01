package com.williambl.haema.vampire.ability.powers.drinking;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContext;
import com.williambl.dfunc.api.context.DFContextSpec;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.dash.DashAbilityPower;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.material.Fluids;

import java.util.List;
import java.util.function.Function;

public record DrinkingAbilityPower(DFunction<Double> cooldown, DFunction<Double> amountToDrink, DFunction<Boolean> canDrink, List<String> keybinds) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<DrinkingAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunction.NUMBER_FUNCTION.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY_TARGET), Function.identity()).fieldOf("cooldown").forGetter(DrinkingAbilityPower::cooldown),
            DFunction.NUMBER_FUNCTION.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY_TARGET), Function.identity()).fieldOf("amount_to_drink").forGetter(DrinkingAbilityPower::amountToDrink),
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY), Function.identity()).fieldOf("can_drink").forGetter(DrinkingAbilityPower::canDrink),
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
        if (!this.canDrink().apply(DFContext.entityTarget(entity, target))) {
            return false;
        }

        var bloodQuality = BloodApi.getBloodQuality(target);
        if (bloodQuality.isEmpty()) {
            return false;
        }

        long amountDroplets = this.amountToDrink().apply(DFContext.entityTarget(entity, target)).longValue();
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
