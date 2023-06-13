package com.williambl.haema.api.ritual;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.api.ritual.module.AraeModule;

import java.util.List;

// arae meaning 'of the altar'. this represents the structure that has the altar in it
public record RitualArae(MultiblockFilter multiblock, List<AraeModule> modules) {
    public static final Codec<RitualArae> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            MultiblockFilter.CODEC.fieldOf("multiblock").forGetter(RitualArae::multiblock),
            AraeModule.MODULE_CODEC.listOf().fieldOf("modules").forGetter(RitualArae::modules)
    ).apply(instance, RitualArae::new));
}
