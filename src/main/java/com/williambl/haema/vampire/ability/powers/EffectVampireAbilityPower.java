package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dpred.DPredicate;
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
            if (effect.predicate().test(entity)) {
                entity.addEffect(effect.createInstance());
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

    private record Data(MobEffect effect, int amplifier, int duration, boolean ambient, boolean showParticles, boolean showIcon, DPredicate<Entity> predicate) {
        private static final Codec<Data> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                BuiltInRegistries.MOB_EFFECT.byNameCodec().fieldOf("effect").forGetter(Data::effect),
                Codec.INT.optionalFieldOf("amplifier", 0).forGetter(Data::amplifier),
                Codec.INT.optionalFieldOf("duration", 0).forGetter(Data::duration),
                Codec.BOOL.optionalFieldOf("ambient", false).forGetter(Data::ambient),
                Codec.BOOL.optionalFieldOf("show_particles", true).forGetter(Data::showParticles),
                Codec.BOOL.optionalFieldOf("show_icon", true).forGetter(Data::showIcon),
                DPredicate.ENTITY_PREDICATE_TYPE_REGISTRY.codec().fieldOf("predicate").forGetter(Data::predicate)
        ).apply(instance, Data::new));

        private MobEffectInstance createInstance() {
            return new MobEffectInstance(this.effect, this.duration, this.amplifier, this.ambient, this.showParticles, this.showIcon);
        }
    }
}
