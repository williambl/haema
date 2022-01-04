package com.williambl.haema.hunter.structure

import net.fabricmc.fabric.api.biome.v1.BiomeModifications
import net.fabricmc.fabric.api.biome.v1.BiomeSelectors
import net.fabricmc.fabric.api.biome.v1.ModificationPhase
import net.fabricmc.fabric.api.structure.v1.FabricStructureBuilder
import net.minecraft.structure.PlainsVillageData
import net.minecraft.util.Identifier
import net.minecraft.util.registry.BuiltinRegistries
import net.minecraft.util.registry.Registry
import net.minecraft.world.gen.GenerationStep
import net.minecraft.world.gen.feature.StructurePoolFeatureConfig

val vampireHunterOutpostFeature = VampireHunterOutpostFeature(StructurePoolFeatureConfig.CODEC)
val configuredVampireHunterOutpostFeature = vampireHunterOutpostFeature.configure(StructurePoolFeatureConfig({ PlainsVillageData.STRUCTURE_POOLS }, 0))

val smallVampireHunterOutpostFeature = SmallVampireHunterOutpostFeature(StructurePoolFeatureConfig.CODEC)
val configuredSmallVampireHunterOutpostFeature = smallVampireHunterOutpostFeature.configure(StructurePoolFeatureConfig({ PlainsVillageData.STRUCTURE_POOLS }, 0))

fun registerStructures() {
    FabricStructureBuilder.create(Identifier("haema:vampire_hunter_outpost"), vampireHunterOutpostFeature)
        .step(GenerationStep.Feature.SURFACE_STRUCTURES)
        .defaultConfig(120, 70, 74426467)
        .enableSuperflat()
        .adjustsSurface()
        .register()

    Registry.register(BuiltinRegistries.CONFIGURED_STRUCTURE_FEATURE, Identifier("haema:configured_vampire_hunter_outpost"), configuredVampireHunterOutpostFeature)

    BiomeModifications.create(Identifier("haema:vampire_hunter_outpost_addition"))
        .add(
            ModificationPhase.ADDITIONS,
            BiomeSelectors.foundInOverworld()
        ) { context -> context.generationSettings.addBuiltInStructure(configuredVampireHunterOutpostFeature) }

    FabricStructureBuilder.create(Identifier("haema:small_vampire_hunter_outpost"), smallVampireHunterOutpostFeature)
        .step(GenerationStep.Feature.SURFACE_STRUCTURES)
        .defaultConfig(100, 60, 74426500)
        .enableSuperflat()
        .adjustsSurface()
        .register()

    Registry.register(BuiltinRegistries.CONFIGURED_STRUCTURE_FEATURE, Identifier("haema:configured_small_vampire_hunter_outpost"), configuredSmallVampireHunterOutpostFeature)

    BiomeModifications.create(Identifier("haema:small_vampire_hunter_outpost_addition"))
        .add(
            ModificationPhase.ADDITIONS,
            BiomeSelectors.foundInOverworld()
        ) { context -> context.generationSettings.addBuiltInStructure(configuredSmallVampireHunterOutpostFeature) }
}