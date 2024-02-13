package com.williambl.haema.vampire.ability.powers.drinking;

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes;
import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.actions.Action;
import com.williambl.actions.Actions;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.haema.api.vampire.ability.powers.drinking.EntityDrinkTargetCallback;
import com.williambl.haema.content.blood.VampireBackedBloodStorage;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.lang.VValue;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.transaction.Transaction;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.projectile.ProjectileUtil;
import net.minecraft.world.phys.EntityHitResult;

import java.util.List;
import java.util.Optional;

public record DrinkingAbilityPower(VExpression amountToDrink, VExpression canDrink, VExpression onDrink, List<String> keybinds) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<DrinkingAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY_TARGET).fieldOf("amount_to_drink").forGetter(DrinkingAbilityPower::amountToDrink),
            DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).fieldOf("can_drink").forGetter(DrinkingAbilityPower::canDrink),
            DFunctions.resolvedExpressionCodec(StandardVTypes.LIST.with(0, Actions.ACTION_TYPE.get()), DFunctions.ENTITY_TARGET).fieldOf("on_drink").forGetter(DrinkingAbilityPower::onDrink),
            Codec.STRING.listOf().fieldOf("keybinds").forGetter(DrinkingAbilityPower::keybinds)
    ).apply(instance, DrinkingAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source, boolean isActive) {
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    public boolean tryDrink(LivingEntity entity, Entity target) {
        if (entity.distanceToSqr(target) > ReachEntityAttributes.getSquaredAttackRange(entity, 3.0)) {
            return false;
        }

        var ctx = DFunctions.createEntityTargetContext(entity, target);
        if (!DFunctions.<Boolean>evaluate(this.canDrink(), ctx)) {
            return false;
        }

        var bloodQuality = BloodApi.getBloodQuality(target);
        if (bloodQuality.isEmpty()) {
            return false;
        }

        long amountDroplets = DFunctions.<Double>evaluate(this.amountToDrink(), ctx).longValue();
        var bloodStorage = BloodApi.getBloodStorage(target);
        if (bloodStorage.isEmpty()) {
            return false;
        }

        var vampireComponent = VampireComponent.KEY.getNullable(entity);
        if (vampireComponent == null) { // should never happen but just in case
            return false;
        }

        var vampireStorage = new VampireBackedBloodStorage(vampireComponent, bloodQuality.get());

        try (var tx = Transaction.openOuter()) {
            var fluid = FluidVariant.of(BloodApi.getFluid(bloodQuality.get()));
            long inserted = vampireStorage.insert(fluid, amountDroplets, tx);
            long extracted = bloodStorage.get().extract(fluid, inserted, tx);
            if (inserted > 0 && extracted == inserted) {
                DFunctions.<List<VValue>>evaluate(this.onDrink(), ctx).stream().map(VValue::<Action>getUnchecked).forEach(Action::runAction);
                tx.commit();
                return true;
            } else {
                tx.abort();
                return false;
            }
        }
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }

    public static void init() {
        EntityDrinkTargetCallback.EVENT.register(vampire -> {
            var hitResult = ProjectileUtil.getHitResultOnViewVector(
                    vampire,
                    entity -> !entity.isSpectator() && entity.isPickable(),
                    ReachEntityAttributes.getAttackRange(vampire, 3.0));
            if (hitResult instanceof EntityHitResult entityHitResult) {
                return Optional.of(entityHitResult.getEntity());
            }

            return Optional.empty();
        });
    }
}