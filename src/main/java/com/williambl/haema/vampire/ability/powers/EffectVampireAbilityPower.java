package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

import java.util.List;
import java.util.Set;

public record EffectVampireAbilityPower(Set<Data> effects) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<EffectVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(Data.CODEC.listOf().fieldOf("effects")
            .xmap(mods -> new EffectVampireAbilityPower(Set.copyOf(mods)), power -> List.copyOf(power.effects)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
        if (entity.level().isClientSide()) {
            return;
        }

        for (var effect : this.effects) {
            if (DFunctions.evaluate(effect.predicate(), DFunctions.createEntityContext(entity))) {
                entity.addEffect(effect.createInstance(entity));
            }
        }
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
        for (var effect : this.effects) {
            entity.removeEffect(effect.effect());
        }
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }

    public record Data(MobEffect effect, VExpression amplifier, VExpression duration, VExpression ambient, VExpression showParticles, VExpression showIcon, VExpression predicate) {
        private static final Codec<Data> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                BuiltInRegistries.MOB_EFFECT.byNameCodec().fieldOf("effect").forGetter(Data::effect),
                DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY).optionalFieldOf("amplifier", VExpression.value(StandardVTypes.NUMBER, 0.)).forGetter(Data::amplifier),
                DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY).optionalFieldOf("duration", VExpression.value(StandardVTypes.NUMBER, (double) 0.)).forGetter(Data::duration),
                DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).optionalFieldOf("ambient", VExpression.value(StandardVTypes.BOOLEAN, false)).forGetter(Data::ambient),
                DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).optionalFieldOf("show_particles", VExpression.value(StandardVTypes.BOOLEAN, true)).forGetter(Data::showParticles),
                DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).optionalFieldOf("show_icon", VExpression.value(StandardVTypes.BOOLEAN, true)).forGetter(Data::showIcon),
                DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY).fieldOf("predicate").forGetter(Data::predicate)
        ).apply(instance, Data::new));

        private MobEffectInstance createInstance(Entity entity) {
            return new MobEffectInstance(this.effect, DFunctions.<Double>evaluate(this.duration, DFunctions.createEntityContext(entity)).intValue(), DFunctions.<Double>evaluate(this.amplifier, DFunctions.createEntityContext(entity)).intValue(), DFunctions.evaluate(this.ambient, DFunctions.createEntityContext(entity)), DFunctions.evaluate(this.showParticles, DFunctions.createEntityContext(entity)), DFunctions.evaluate(this.showIcon, DFunctions.createEntityContext(entity)));
        }
    }
}
