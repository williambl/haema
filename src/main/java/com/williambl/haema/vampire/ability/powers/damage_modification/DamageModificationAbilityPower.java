package com.williambl.haema.vampire.ability.powers.damage_modification;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

import java.util.function.Function;

public record DamageModificationAbilityPower(DFunction<Double> damageModificationFunction, DFunction<Boolean> canDamageKillFunction) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<DamageModificationAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunction.NUMBER_FUNCTION.codec().comapFlatMap(HaemaUtil.verifyDFunction(HaemaDFunctions.ENTITY_DAMAGE_WITH_WEAPON), Function.identity()).fieldOf("damage_modification").forGetter(DamageModificationAbilityPower::damageModificationFunction),
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(HaemaDFunctions.ENTITY_DAMAGE_WITH_WEAPON), Function.identity()).fieldOf("can_damage_kill").forGetter(DamageModificationAbilityPower::canDamageKillFunction)
    ).apply(instance, DamageModificationAbilityPower::new)));

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
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }

    public static void init() {
        DamageModificationCallback.EVENT.register((source, amount, entity) -> {
            var component = VampireAbilitiesComponent.KEY.getNullable(entity);
            float workingAmount = amount;
            if (component != null) {
                var context = HaemaDFunctions.entityDamageWithWeapon(entity, source, amount, ((ExtendedDamageSource)source).weapon());
                for (var power : component.getPowersOfClass(DamageModificationAbilityPower.class)) {
                    workingAmount = power.damageModificationFunction.apply(context).floatValue();
                }
            }

            return workingAmount;
        });

        CheatDeathCallback.EVENT.register((source, amount, entity) -> {
            var component = VampireAbilitiesComponent.KEY.getNullable(entity);
            if (component != null) {
                var context = HaemaDFunctions.entityDamageWithWeapon(entity, source, amount, ((ExtendedDamageSource)source).weapon());
                for (var power : component.getPowersOfClass(DamageModificationAbilityPower.class)) {
                    if (!power.canDamageKillFunction.apply(context)) {
                        entity.setHealth(1.0f);
                        return true;
                    }
                }
            }

            return false;
        });
    }
}
