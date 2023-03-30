package com.williambl.haema.vampire.ability;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;

import java.util.List;
import java.util.Set;

import static com.williambl.haema.Haema.id;

public record VampireAbility(boolean enabled,
                             boolean isAlwaysActive,
                             boolean enabledByDefault,
                             Set<ResourceKey<VampireAbility>> prerequisites,
                             Set<ResourceKey<VampireAbility>> conflicts,
                             Set<VampireAbilityPower> powers) {

    public static final ResourceKey<Registry<VampireAbility>> REGISTRY_KEY = ResourceKey.createRegistryKey(id( "vampire_ability"));

    public static final Codec<VampireAbility> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.BOOL.fieldOf("enabled").forGetter(VampireAbility::enabled),
            Codec.BOOL.fieldOf("is_always_active").forGetter(VampireAbility::isAlwaysActive),
            Codec.BOOL.fieldOf("enabled_by_default").forGetter(VampireAbility::enabledByDefault),
            ResourceKey.codec(REGISTRY_KEY).listOf().fieldOf("prerequisites").xmap(Set::copyOf, List::copyOf).forGetter(VampireAbility::prerequisites),
            ResourceKey.codec(REGISTRY_KEY).listOf().fieldOf("conflicts").xmap(Set::copyOf, List::copyOf).forGetter(VampireAbility::conflicts),
            VampireAbilityPower.POWER_CODEC.listOf().fieldOf("powers").xmap(Set::copyOf, List::copyOf).forGetter(VampireAbility::powers)
    ).apply(instance, VampireAbility::new));
}
