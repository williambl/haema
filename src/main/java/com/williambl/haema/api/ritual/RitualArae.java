package com.williambl.haema.api.ritual;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.api.ritual.module.AraeModule;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;

import java.util.List;

import static com.williambl.haema.Haema.id;

/**
 * A ritual arae is a multiblock structure that can be used to perform rituals. These are defined in datapacks.
 * One instance of {@code RitualArae} represents one type of Arae, like how one instance of {@code Block} represents
 * one type of block.
 * @param multiblock    the multiblock filter for this arae
 * @param modules       the modules for this arae
 */
public record RitualArae(MultiblockFilter multiblock, List<AraeModule> modules) {
    public static final ResourceKey<Registry<RitualArae>> REGISTRY_KEY = ResourceKey.createRegistryKey(id( "ritual_arae"));

    public static final Codec<RitualArae> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            MultiblockFilter.CODEC.fieldOf("multiblock").forGetter(RitualArae::multiblock),
            AraeModule.MODULE_CODEC.listOf().fieldOf("modules").forGetter(RitualArae::modules)
    ).apply(instance, RitualArae::new));
}
