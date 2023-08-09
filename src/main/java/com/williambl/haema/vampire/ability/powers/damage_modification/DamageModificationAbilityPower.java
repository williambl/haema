package com.williambl.haema.vampire.ability.powers.damage_modification;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

public record DamageModificationAbilityPower(VExpression damageModificationFunction, VExpression canDamageKillFunction) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<DamageModificationAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, HaemaDFunctions.ENTITY_DAMAGE_WITH_WEAPON).fieldOf("damage_modification").forGetter(DamageModificationAbilityPower::damageModificationFunction),
            DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, HaemaDFunctions.ENTITY_DAMAGE_WITH_WEAPON).fieldOf("can_damage_kill").forGetter(DamageModificationAbilityPower::canDamageKillFunction)
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
                    workingAmount = DFunctions.<Double>evaluate(power.damageModificationFunction, context).floatValue();
                }
            }

            return workingAmount;
        });

        CheatDeathCallback.EVENT.register((source, amount, entity) -> {
            var component = VampireAbilitiesComponent.KEY.getNullable(entity);
            if (component != null) {
                var context = HaemaDFunctions.entityDamageWithWeapon(entity, source, amount, ((ExtendedDamageSource)source).weapon());
                for (var power : component.getPowersOfClass(DamageModificationAbilityPower.class)) {
                    if (!DFunctions.<Boolean>evaluate(power.canDamageKillFunction, context)) {
                        entity.setHealth(1.0f);
                        return true;
                    }
                }
            }

            return false;
        });
    }
}
