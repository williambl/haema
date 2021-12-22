package com.williambl.haema.hunter.structure

import com.mojang.serialization.Codec
import net.minecraft.structure.MarginedStructureStart
import net.minecraft.structure.PoolStructurePiece
import net.minecraft.structure.StructureManager
import net.minecraft.structure.pool.StructurePoolBasedGenerator
import net.minecraft.util.Identifier
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.ChunkPos
import net.minecraft.util.math.Vec3i
import net.minecraft.util.registry.DynamicRegistryManager
import net.minecraft.util.registry.Registry
import net.minecraft.world.HeightLimitView
import net.minecraft.world.Heightmap
import net.minecraft.world.biome.Biome
import net.minecraft.world.biome.source.BiomeSource
import net.minecraft.world.gen.ChunkRandom
import net.minecraft.world.gen.chunk.ChunkGenerator
import net.minecraft.world.gen.feature.DefaultFeatureConfig
import net.minecraft.world.gen.feature.StructureFeature
import net.minecraft.world.gen.feature.StructureFeature.StructureStartFactory
import net.minecraft.world.gen.feature.StructurePoolFeatureConfig

class SmallVampireHunterOutpostFeature(codec: Codec<DefaultFeatureConfig>) : StructureFeature<DefaultFeatureConfig>(codec) {
    override fun getStructureStartFactory(): StructureStartFactory<DefaultFeatureConfig>
            = StructureStartFactory(::Start)

    override fun shouldStartAt(
        chunkGenerator: ChunkGenerator,
        biomeSource: BiomeSource,
        worldSeed: Long,
        random: ChunkRandom,
        chunkPos: ChunkPos,
        biome: Biome,
        chunkPos2: ChunkPos,
        config: DefaultFeatureConfig,
        world: HeightLimitView
    ): Boolean {
        val centerPos = BlockPos(chunkPos.centerX, 0, chunkPos.centerZ)
        val averageHeightAround = (-1..1).asSequence()
            .flatMap { x ->(-1..1).map { z -> x to z } }
            .map { (x, z) -> centerPos.x + x * 16 to centerPos.z + z * 16 }
            .map { (x, z) -> chunkGenerator.getHeight(x, z, Heightmap.Type.WORLD_SURFACE_WG, world) }
            .average()
        val landHeight = chunkGenerator.getHeightInGround(centerPos.x, centerPos.z, Heightmap.Type.WORLD_SURFACE_WG, world)

        return chunkGenerator.getColumnSample(centerPos.x, centerPos.z, world).getState(centerPos.up(landHeight)).fluidState.isEmpty
                && random.nextDouble() < 0.3 * ((landHeight - averageHeightAround) + 4) * (landHeight/120.0)
    }

    class Start(
        feature: StructureFeature<DefaultFeatureConfig>?,
        chunkPos: ChunkPos,
        references: Int,
        seed: Long
    ) : MarginedStructureStart<DefaultFeatureConfig>(feature, chunkPos, references, seed) {
        override fun init(
            registryManager: DynamicRegistryManager,
            chunkGenerator: ChunkGenerator,
            manager: StructureManager,
            chunkPos: ChunkPos,
            biome: Biome,
            config: DefaultFeatureConfig,
            world: HeightLimitView
        ) {
            val x = chunkPos.centerX
            val z = chunkPos.centerZ
            val centre = BlockPos(x, 0, z)

            StructurePoolBasedGenerator.generate(
                registryManager,
                StructurePoolFeatureConfig({
                    registryManager.get(Registry.STRUCTURE_POOL_KEY).get(
                    Identifier("haema:small_vampire_hunter_outpost/start_pool"))}, 10
                ),
                { structureManager, poolElement, pos, i, rot, bounds -> PoolStructurePiece(structureManager, poolElement, pos, i, rot, bounds) },
                chunkGenerator,
                manager,
                centre,
                this,
                random,
                false,
                true,
                world
            )


            val structureCenter: Vec3i = children[0].boundingBox.center
            val xOffset: Int = x - structureCenter.x
            val zOffset: Int = z - structureCenter.z
            for (structurePiece in children) {
                structurePiece.translate(xOffset, 0, zOffset)
            }

            setBoundingBoxFromChildren()
        }

    }

}