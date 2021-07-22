package com.williambl.haema.hunter.structure

import com.mojang.serialization.Codec
import net.minecraft.structure.MarginedStructureStart
import net.minecraft.structure.PoolStructurePiece
import net.minecraft.structure.StructureManager
import net.minecraft.structure.pool.StructurePoolBasedGenerator
import net.minecraft.util.Identifier
import net.minecraft.util.math.BlockBox
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.ChunkPos
import net.minecraft.util.registry.DynamicRegistryManager
import net.minecraft.util.registry.Registry
import net.minecraft.world.Heightmap
import net.minecraft.world.biome.Biome
import net.minecraft.world.biome.source.BiomeSource
import net.minecraft.world.gen.ChunkRandom
import net.minecraft.world.gen.chunk.ChunkGenerator
import net.minecraft.world.gen.feature.DefaultFeatureConfig
import net.minecraft.world.gen.feature.StructureFeature
import net.minecraft.world.gen.feature.StructureFeature.StructureStartFactory
import net.minecraft.world.gen.feature.StructurePoolFeatureConfig

//TODO: entities
//TODO: one layer down
//TODO: structure voids
class MountainVampireHunterOutpostFeature(codec: Codec<DefaultFeatureConfig>) : StructureFeature<DefaultFeatureConfig>(codec) {
    override fun getStructureStartFactory(): StructureStartFactory<DefaultFeatureConfig>
            = StructureStartFactory { feature, chunkX, chunkZ, box, referenceCount, worldSeed -> Start(
        feature,
        chunkX,
        chunkZ,
        box,
        referenceCount,
        worldSeed
    )}

    override fun shouldStartAt(
        chunkGenerator: ChunkGenerator,
        biomeSource: BiomeSource,
        worldSeed: Long,
        random: ChunkRandom,
        chunkX: Int,
        chunkZ: Int,
        biome: Biome,
        chunkPos: ChunkPos,
        config: DefaultFeatureConfig
    ): Boolean {
        val centerPos = BlockPos((chunkX shl 4) + 7, 0, (chunkZ shl 4) + 7)
        val averageHeightAround = (-1..1).asSequence()
            .flatMap { x ->(-1..1).map { z -> x to z } }
            .map { (x, z) -> centerPos.x + x * 16 to centerPos.z + z * 16 }
            .map { (x, z) -> chunkGenerator.getHeight(x, z, Heightmap.Type.WORLD_SURFACE_WG) }
            .average()
        val landHeight = chunkGenerator.getHeightInGround(centerPos.x, centerPos.z, Heightmap.Type.WORLD_SURFACE_WG)

        return chunkGenerator.getColumnSample(centerPos.x, centerPos.z).getBlockState(centerPos.up(landHeight)).fluidState.isEmpty
                && random.nextDouble() < 0.3 * ((landHeight - averageHeightAround) + 10) * (landHeight/80.0)
    }

    class Start(
        feature: StructureFeature<DefaultFeatureConfig>?,
        chunkX: Int,
        chunkZ: Int,
        box: BlockBox,
        references: Int,
        seed: Long
    ) : MarginedStructureStart<DefaultFeatureConfig>(feature, chunkX, chunkZ, box, references, seed) {
        override fun init(
            registryManager: DynamicRegistryManager,
            chunkGenerator: ChunkGenerator,
            manager: StructureManager,
            chunkX: Int,
            chunkZ: Int,
            biome: Biome,
            config: DefaultFeatureConfig
        ) {
            val x = (chunkX shl 4) + 7
            val z = (chunkZ shl 4) + 7
            val mutablePos = BlockPos.Mutable(x, 0, z)

            StructurePoolBasedGenerator.method_30419(
                registryManager,
                StructurePoolFeatureConfig({
                    registryManager.get(Registry.TEMPLATE_POOL_WORLDGEN).get(
                    Identifier("haema:mountain_vampire_hunter_outpost/start_pool"))}, 10
                ),
                { structureManager, poolElement, pos, i, rot, bounds -> PoolStructurePiece(structureManager, poolElement, pos, i, rot, bounds) },
                chunkGenerator,
                manager,
                mutablePos,
                children,
                random,
                false,
                true
            )

            setBoundingBoxFromChildren()
        }

    }

}