package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.DFunction;
import com.williambl.dfunc.number.EntityNumberDFunctions;
import com.williambl.dfunc.predicate.EntityDPredicates;
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
            if (effect.predicate().apply(entity)) {
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

    public record Data(MobEffect effect, DFunction<Entity, Double> amplifier, DFunction<Entity, Double> duration, DFunction<Entity, Boolean> ambient, DFunction<Entity, Boolean> showParticles, DFunction<Entity, Boolean> showIcon, DFunction<Entity, Boolean> predicate) {
        private static final Codec<Data> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                BuiltInRegistries.MOB_EFFECT.byNameCodec().fieldOf("effect").forGetter(Data::effect),
                DFunction.ENTITY_TO_NUMBER_FUNCTION_TYPE_REGISTRY.codec().optionalFieldOf("amplifier", EntityNumberDFunctions.CONSTANT.factory().apply(0.)).forGetter(Data::amplifier),
                DFunction.ENTITY_TO_NUMBER_FUNCTION_TYPE_REGISTRY.codec().optionalFieldOf("duration", EntityNumberDFunctions.CONSTANT.factory().apply(0.)).forGetter(Data::duration),
                DFunction.ENTITY_PREDICATE_TYPE_REGISTRY.codec().optionalFieldOf("ambient", EntityDPredicates.CONSTANT.factory().apply(false)).forGetter(Data::ambient),
                DFunction.ENTITY_PREDICATE_TYPE_REGISTRY.codec().optionalFieldOf("show_particles", EntityDPredicates.CONSTANT.factory().apply(true)).forGetter(Data::showParticles),
                DFunction.ENTITY_PREDICATE_TYPE_REGISTRY.codec().optionalFieldOf("show_icon", EntityDPredicates.CONSTANT.factory().apply(true)).forGetter(Data::showIcon),
                DFunction.ENTITY_PREDICATE_TYPE_REGISTRY.codec().fieldOf("predicate").forGetter(Data::predicate)
        ).apply(instance, Data::new));

        private MobEffectInstance createInstance(Entity entity) {
            return new MobEffectInstance(this.effect, this.duration.apply(entity).intValue(), this.amplifier.apply(entity).intValue(), this.ambient.apply(entity), this.showParticles.apply(entity), this.showIcon.apply(entity));
        }
    }
}
