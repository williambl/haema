package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.vampire.VampireComponent;
import com.williambl.haema.vampire.ability.VampireAbility;
import com.williambl.haema.vampire.ability.VampireAbilityPower;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.effect.MobEffectInstance;
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
            double blood = entity.getComponent(VampireComponent.KEY).getBlood();
            if (blood >= effect.minBloodInclusive() && blood < effect.maxBloodExclusive()) {
                entity.addEffect(effect.effect());
            }
        }
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
        for (var effect : this.effects) {
            entity.removeEffect(effect.effect().getEffect());
        }
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }

    private record Data(MobEffectInstance effect, double minBloodInclusive, double maxBloodExclusive) {
        private static final Codec<Data> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                HaemaUtil.MOB_EFFECT_INSTANCE_CODEC.fieldOf("effect").forGetter(Data::effect),
                Codec.DOUBLE.optionalFieldOf("min_blood_inclusive", Double.NEGATIVE_INFINITY).forGetter(Data::minBloodInclusive),
                Codec.DOUBLE.optionalFieldOf("max_blood_exclusive", Double.POSITIVE_INFINITY).forGetter(Data::maxBloodExclusive)
        ).apply(instance, Data::new));
    }
}
