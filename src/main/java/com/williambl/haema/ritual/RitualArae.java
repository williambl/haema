package com.williambl.haema.ritual;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.HaemaUtil;
import net.minecraft.core.particles.ParticleOptions;
import net.minecraft.core.particles.ParticleTypes;

import java.util.List;

// arae meaning 'of the altar'. this represents the structure that has the altar in it
public record RitualArae(MultiblockFilter multiblock, List<Character> particleSourceChars, ParticleOptions particles) {
    public static final Codec<RitualArae> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            MultiblockFilter.CODEC.fieldOf("multiblock").forGetter(RitualArae::multiblock),
            HaemaUtil.CHARACTER_CODEC.listOf().fieldOf("particle_source_chars").forGetter(RitualArae::particleSourceChars),
            ParticleTypes.CODEC.fieldOf("particles").forGetter(RitualArae::particles)
    ).apply(instance, RitualArae::new));
}
