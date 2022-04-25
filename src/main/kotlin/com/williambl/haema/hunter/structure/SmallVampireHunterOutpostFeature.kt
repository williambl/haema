package com.williambl.haema.hunter.structure

import com.mojang.serialization.Codec
import com.williambl.haema.id
import net.minecraft.structure.PoolStructurePiece
import net.minecraft.structure.StructureGeneratorFactory
import net.minecraft.structure.StructurePiecesGenerator
import net.minecraft.structure.pool.StructurePoolBasedGenerator
import net.minecraft.util.registry.Registry
import net.minecraft.util.registry.RegistryKey
import net.minecraft.world.Heightmap
import net.minecraft.world.gen.feature.StructureFeature
import net.minecraft.world.gen.feature.StructurePoolFeatureConfig
import java.util.*

class SmallVampireHunterOutpostFeature(codec: Codec<StructurePoolFeatureConfig>) : StructureFeature<StructurePoolFeatureConfig>(
    codec,
    { ctx ->
        if (!canGenerate(ctx)) {
            Optional.empty<StructurePiecesGenerator<StructurePoolFeatureConfig>>()
        } else {
            createPiecesGenerator(ctx)
        }
    }
) {

    companion object {
        fun canGenerate(ctx: StructureGeneratorFactory.Context<StructurePoolFeatureConfig>): Boolean {
            val centerPos = ctx.chunkPos.getCenterAtY(0)
            val averageHeightAround = (-1..1).asSequence()
                .flatMap { x ->(-1..1).map { z -> x to z } }
                .map { (x, z) -> centerPos.x + x * 16 to centerPos.z + z * 16 }
                .map { (x, z) -> ctx.chunkGenerator.getHeight(x, z, Heightmap.Type.WORLD_SURFACE_WG, ctx.world) }
                .average()
            val landHeight = ctx.chunkGenerator.getHeightInGround(centerPos.x, centerPos.z, Heightmap.Type.WORLD_SURFACE_WG, ctx.world)

            return ctx.chunkGenerator.getColumnSample(centerPos.x, centerPos.z, ctx.world).getState(landHeight).fluidState.isEmpty
                    && 0.3 * ((landHeight - averageHeightAround) + 4) * (landHeight/120.0) > 0.5
        }

        fun createPiecesGenerator(ctx: StructureGeneratorFactory.Context<StructurePoolFeatureConfig>): Optional<StructurePiecesGenerator<StructurePoolFeatureConfig>> {
            val centre = ctx.chunkPos.getCenterAtY(0)

            val config = StructurePoolFeatureConfig(
                ctx.registryManager.get(Registry.STRUCTURE_POOL_KEY).entryOf(RegistryKey.of(Registry.STRUCTURE_POOL_KEY, id("small_vampire_hunter_outpost/start_pool"))),
                10
            )

            val newCtx = StructureGeneratorFactory.Context(
                ctx.chunkGenerator,
                ctx.biomeSource,
                ctx.seed,
                ctx.chunkPos,
                config,
                ctx.world,
                ctx.validBiome,
                ctx.structureManager,
                ctx.registryManager
            )

            return StructurePoolBasedGenerator.generate(
                newCtx,
                ::PoolStructurePiece,
                centre,
                false,
                true
            )
        }
    }
}