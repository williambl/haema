package com.williambl.haema.vampire.ability.powers;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attribute;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;

import java.util.List;
import java.util.Set;

public record AttributeVampireAbilityPower(Set<Data> modifiers) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<AttributeVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(Data.CODEC.listOf().fieldOf("effects")
            .xmap(mods -> new AttributeVampireAbilityPower(Set.copyOf(mods)), power -> List.copyOf(power.modifiers)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
        for (var modifier : this.modifiers) {
            double blood = entity.getComponent(VampireComponent.KEY).getBlood();
            var attr = entity.getAttribute(modifier.attribute());
            if (attr == null) {
                continue;
            }
            if (attr.hasModifier(modifier.modifier()) && (blood < modifier.minBloodInclusive() || blood >= modifier.maxBloodExclusive())) {
                attr.removeModifier(modifier.modifier());
            } else if (!attr.hasModifier(modifier.modifier()) && blood >= modifier.minBloodInclusive() && blood < modifier.maxBloodExclusive()) {
                attr.addTransientModifier(modifier.modifier());
            }
        }
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
        for (var modifier : this.modifiers) {
            var attr = entity.getAttribute(modifier.attribute());
            if (attr == null) {
                continue;
            }
            attr.removeModifier(modifier.modifier());
        }
    }

    @Override
    public KeyDispatchDataCodec<? extends VampireAbilityPower> codec() {
        return CODEC;
    }

    public record Data(Attribute attribute, AttributeModifier modifier, double minBloodInclusive, double maxBloodExclusive) {
        private static final Codec<Data> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                BuiltInRegistries.ATTRIBUTE.byNameCodec().fieldOf("attribute").forGetter(Data::attribute),
                HaemaUtil.ATTRIBUTE_MODIFIER_CODEC.fieldOf("modifier").forGetter(Data::modifier),
                Codec.DOUBLE.optionalFieldOf("min_blood_inclusive", Double.NEGATIVE_INFINITY).forGetter(Data::minBloodInclusive),
                Codec.DOUBLE.optionalFieldOf("max_blood_exclusive", Double.POSITIVE_INFINITY).forGetter(Data::maxBloodExclusive)
        ).apply(instance, Data::new));
    }
}
