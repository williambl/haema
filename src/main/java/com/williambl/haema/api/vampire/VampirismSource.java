package com.williambl.haema.api.vampire;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContextSpec;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceKey;

import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static com.williambl.haema.Haema.id;

/**
 * A source of vampirism. This is a way of becoming a vampire, or a way of being cured.
 * A source can be cured by a set of other sources.
 * A source has a set of abilities which are given to players when being converted.
 */
public record VampirismSource(Set<ResourceKey<VampirismSource>> canBeCuredBy, Set<ResourceKey<VampireAbility>> grantedAbilities, DFunction<Boolean> canConvert, DFunction<Boolean> canCure) {
    public static final ResourceKey<Registry<VampirismSource>> REGISTRY_KEY = ResourceKey.createRegistryKey(id("vampirism_source"));

    public static final Codec<VampirismSource> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            ResourceKey.codec(REGISTRY_KEY).listOf().xmap(Set::copyOf, List::copyOf).fieldOf("can_be_cured_by").forGetter(VampirismSource::canBeCuredBy),
            ResourceKey.codec(VampireAbility.REGISTRY_KEY).listOf().xmap(Set::copyOf, List::copyOf).fieldOf("granted_abilities").forGetter(VampirismSource::grantedAbilities),
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY), Function.identity()).fieldOf("can_convert").forGetter(VampirismSource::canConvert),
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY), Function.identity()).fieldOf("can_cure").forGetter(VampirismSource::canCure)
    ).apply(instance, VampirismSource::new));

    /**
     * Whether vampirism from this source can be cured by another source.
     * @param registries    a registry access
     * @param cure          the possible cure
     * @return              whether this source can be cured by the other source
     */
    public boolean canBeCuredBy(RegistryAccess registries, VampirismSource cure) {
        return registries.registry(REGISTRY_KEY)
                .flatMap(r -> r.getResourceKey(cure))
                .map(this.canBeCuredBy()::contains)
                .orElse(false);
    }
}
