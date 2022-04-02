package com.williambl.haema.hunter

import com.williambl.haema.hunter.structure.SmallVampireHunterOutpostFeature
import com.williambl.haema.hunter.structure.VampireHunterOutpostFeature
import com.williambl.haema.id
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricDefaultAttributeRegistry
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricEntityTypeBuilder
import net.fabricmc.fabric.api.biome.v1.BiomeModifications
import net.fabricmc.fabric.api.biome.v1.BiomeSelectors
import net.fabricmc.fabric.api.biome.v1.ModificationPhase
import net.fabricmc.fabric.api.structure.v1.FabricStructureBuilder
import net.minecraft.entity.EntityDimensions
import net.minecraft.entity.EntityType
import net.minecraft.entity.SpawnGroup
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.mob.HostileEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemGroup
import net.minecraft.structure.PlainsVillageData
import net.minecraft.util.registry.BuiltinRegistries
import net.minecraft.util.registry.Registry
import net.minecraft.world.biome.Biome
import net.minecraft.world.gen.GenerationStep
import net.minecraft.world.gen.feature.ConfiguredStructureFeature
import net.minecraft.world.gen.feature.StructureFeature
import net.minecraft.world.gen.feature.StructurePoolFeatureConfig

object VampireHunterModule: ModInitializer {
    val VAMPIRE_HUNTER: EntityType<VampireHunterEntity> =
        Registry.register(
            Registry.ENTITY_TYPE,
            id("vampire_hunter"),
            FabricEntityTypeBuilder.create<VampireHunterEntity>(SpawnGroup.CREATURE) { type, world -> VampireHunterEntity(type, world) }
                .dimensions(EntityDimensions.fixed(0.6f, 1.95f))
                .trackRangeBlocks(128).trackedUpdateRate(3).spawnableFarFromPlayer().build()
        )

    val VAMPIRE_HUNTER_CONTRACT: VampireHunterContract = Registry.register(
        Registry.ITEM,
        id("vampire_hunter_contract"),
        VampireHunterContract(Item.Settings().group(ItemGroup.MISC))
    )

    val VAMPIRE_HUNTER_OUTPOST_FEATURE = VampireHunterOutpostFeature(StructurePoolFeatureConfig.CODEC)
    val CONFIGURED_VAMPIRE_HUNTER_OUTPOST_FEATURE: ConfiguredStructureFeature<StructurePoolFeatureConfig, out StructureFeature<StructurePoolFeatureConfig>>
            = VAMPIRE_HUNTER_OUTPOST_FEATURE.configure(
        StructurePoolFeatureConfig({ PlainsVillageData.STRUCTURE_POOLS }, 0)
    )

    val SMALL_VAMPIRE_HUNTER_OUTPOST_FEATURE = SmallVampireHunterOutpostFeature(StructurePoolFeatureConfig.CODEC)
    val CONFIGURED_SMALL_VAMPIRE_HUNTER_OUTPOST_FEATURE: ConfiguredStructureFeature<StructurePoolFeatureConfig, out StructureFeature<StructurePoolFeatureConfig>>
            = SMALL_VAMPIRE_HUNTER_OUTPOST_FEATURE.configure(
        StructurePoolFeatureConfig({ PlainsVillageData.STRUCTURE_POOLS }, 0)
    )

    override fun onInitialize() {
        FabricDefaultAttributeRegistry.register(
            VAMPIRE_HUNTER,
            HostileEntity.createHostileAttributes().add(EntityAttributes.GENERIC_MOVEMENT_SPEED, 0.35)
                .add(EntityAttributes.GENERIC_MAX_HEALTH, 20.0)
                .add(EntityAttributes.GENERIC_ATTACK_DAMAGE, 5.0)
                .add(EntityAttributes.GENERIC_FOLLOW_RANGE, 64.0)
        )

        FabricStructureBuilder.create(id("vampire_hunter_outpost"), VAMPIRE_HUNTER_OUTPOST_FEATURE)
            .step(GenerationStep.Feature.SURFACE_STRUCTURES)
            .defaultConfig(120, 70, 74426467)
            .enableSuperflat()
            .adjustsSurface()
            .register()

        Registry.register(BuiltinRegistries.CONFIGURED_STRUCTURE_FEATURE, id("configured_vampire_hunter_outpost"), CONFIGURED_VAMPIRE_HUNTER_OUTPOST_FEATURE)

        BiomeModifications.create(id("vampire_hunter_outpost_addition"))
            .add(
                ModificationPhase.ADDITIONS,
                BiomeSelectors.foundInOverworld()
            ) { context -> context.generationSettings.addBuiltInStructure(CONFIGURED_VAMPIRE_HUNTER_OUTPOST_FEATURE) }

        FabricStructureBuilder.create(id("small_vampire_hunter_outpost"), SMALL_VAMPIRE_HUNTER_OUTPOST_FEATURE)
            .step(GenerationStep.Feature.SURFACE_STRUCTURES)
            .defaultConfig(100, 60, 74426500)
            .enableSuperflat()
            .adjustsSurface()
            .register()

        Registry.register(BuiltinRegistries.CONFIGURED_STRUCTURE_FEATURE, id("configured_small_vampire_hunter_outpost"), CONFIGURED_SMALL_VAMPIRE_HUNTER_OUTPOST_FEATURE)

        BiomeModifications.create(id("small_vampire_hunter_outpost_addition"))
            .add(
                ModificationPhase.ADDITIONS,
                BiomeSelectors.foundInOverworld()
            ) { context -> context.generationSettings.addBuiltInStructure(CONFIGURED_SMALL_VAMPIRE_HUNTER_OUTPOST_FEATURE) }
    }
}