package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.DFContext;
import com.williambl.dfunc.DFunction;
import com.williambl.dfunc.functions.DPredicates;
import com.williambl.dfunc.functions.NumberDFunctions;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
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
        for (var effect : this.effects) {
            if (effect.predicate().apply(DFContext.entity(entity))) {
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

    public record Data(MobEffect effect, DFunction<Double> amplifier, DFunction<Double> duration, DFunction<Boolean> ambient, DFunction<Boolean> showParticles, DFunction<Boolean> showIcon, DFunction<Boolean> predicate) {
        private static final Codec<Data> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                BuiltInRegistries.MOB_EFFECT.byNameCodec().fieldOf("effect").forGetter(Data::effect),
                DFunction.NUMBER_FUNCTION.codec().optionalFieldOf("amplifier", NumberDFunctions.CONSTANT.factory().apply(0.)).forGetter(Data::amplifier),
                DFunction.NUMBER_FUNCTION.codec().optionalFieldOf("duration", NumberDFunctions.CONSTANT.factory().apply(0.)).forGetter(Data::duration),
                DFunction.PREDICATE.codec().optionalFieldOf("ambient", DPredicates.CONSTANT.factory().apply(false)).forGetter(Data::ambient),
                DFunction.PREDICATE.codec().optionalFieldOf("show_particles", DPredicates.CONSTANT.factory().apply(true)).forGetter(Data::showParticles),
                DFunction.PREDICATE.codec().optionalFieldOf("show_icon", DPredicates.CONSTANT.factory().apply(true)).forGetter(Data::showIcon),
                DFunction.PREDICATE.codec().fieldOf("predicate").forGetter(Data::predicate)
        ).apply(instance, Data::new));

        private MobEffectInstance createInstance(Entity entity) {
            return new MobEffectInstance(this.effect, this.duration.apply(DFContext.entity(entity)).intValue(), this.amplifier.apply(DFContext.entity(entity)).intValue(), this.ambient.apply(DFContext.entity(entity)), this.showParticles.apply(DFContext.entity(entity)), this.showIcon.apply(DFContext.entity(entity)));
        }
    }
}
