package com.williambl.haema.hunter.structure

import net.fabricmc.fabric.api.biome.v1.BiomeModifications
import net.fabricmc.fabric.api.biome.v1.BiomeSelectors
import net.fabricmc.fabric.api.biome.v1.ModificationPhase
import net.fabricmc.fabric.api.structure.v1.FabricStructureBuilder
import net.minecraft.util.Identifier
import net.minecraft.util.registry.BuiltinRegistries
import net.minecraft.util.registry.Registry
import net.minecraft.world.biome.Biome
import net.minecraft.world.gen.GenerationStep
import net.minecraft.world.gen.feature.DefaultFeatureConfig
import net.minecraft.world.gen.feature.FeatureConfig

val vampireHunterOutpostFeature = VampireHunterOutpostFeature(DefaultFeatureConfig.CODEC)
val configuredVampireHunterOutpostFeature = vampireHunterOutpostFeature.configure(FeatureConfig.DEFAULT)

fun registerStructures() {
    FabricStructureBuilder.create(Identifier("haema:vampire_hunter_outpost"), vampireHunterOutpostFeature)
        .step(GenerationStep.Feature.SURFACE_STRUCTURES)
        .defaultConfig(60, 30, 74426467)
        .superflatFeature(vampireHunterOutpostFeature.configure(FeatureConfig.DEFAULT))
        .adjustsSurface()
        .register()

    Registry.register(BuiltinRegistries.CONFIGURED_STRUCTURE_FEATURE, Identifier("haema:configured_vampire_hunter_outpost"), configuredVampireHunterOutpostFeature)

    BiomeModifications.create(Identifier("haema:vampire_hunter_outpost_addition"))
        .add(
            ModificationPhase.ADDITIONS,
            BiomeSelectors.categories(Biome.Category.DESERT, Biome.Category.EXTREME_HILLS, Biome.Category.FOREST, Biome.Category.MESA, Biome.Category.PLAINS, Biome.Category.SAVANNA),
            { context -> context.generationSettings.addBuiltInStructure(configuredVampireHunterOutpostFeature)}
        )
}