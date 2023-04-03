package com.williambl.haema.api.vampire;

import com.google.common.graph.Graph;
import com.google.common.graph.GraphBuilder;
import com.google.common.graph.MutableGraph;
import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceKey;

import java.util.List;
import java.util.Set;

import static com.williambl.haema.Haema.id;

/**
 * A source of vampirism. This is a way of becoming a vampire, or a way of being cured.
 * This exists so that different methods of becoming a vampire can only be cured in certain ways.
 */
public record VampirismSource(Set<ResourceKey<VampirismSource>> canBeCuredBy) {
    public static final ResourceKey<Registry<VampirismSource>> REGISTRY_KEY = ResourceKey.createRegistryKey(id("vampirism_source"));

    public static final Codec<VampirismSource> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            ResourceKey.codec(REGISTRY_KEY).listOf().xmap(Set::copyOf, List::copyOf).fieldOf("can_be_cured_by").forGetter(VampirismSource::canBeCuredBy)
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
