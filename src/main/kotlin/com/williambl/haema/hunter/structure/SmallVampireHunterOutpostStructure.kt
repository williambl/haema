package com.williambl.haema.hunter.structure

import com.mojang.serialization.Codec
import com.mojang.serialization.codecs.RecordCodecBuilder
import com.williambl.haema.hunter.VampireHunterModule
import net.minecraft.structure.pool.StructurePool
import net.minecraft.structure.pool.StructurePoolBasedGenerator
import net.minecraft.util.Identifier
import net.minecraft.util.math.BlockPos
import net.minecraft.util.registry.RegistryEntry
import net.minecraft.world.Heightmap
import net.minecraft.world.gen.heightprovider.HeightProvider
import net.minecraft.world.gen.structure.Structure
import net.minecraft.world.gen.structure.StructureType
import java.util.*

class SmallVampireHunterOutpostStructure(
    config: Config,
    val startPool: RegistryEntry<StructurePool>,
    val startJigsawName: Optional<Identifier>,
    val size: Int,
    val startHeight: HeightProvider,
    val projectStartToHeightmap: Optional<Heightmap.Type>,
    val maxDistanceFromCentre: Int
) : Structure(
    config
) {

    override fun getStructurePosition(context: Context): Optional<StructurePosition> {
        if (!canGenerate(context)) {
            return Optional.empty()
        }

        // Turns the chunk coordinates into actual coordinates we can use. (Gets corner of that chunk)
        val chunkPos = context.chunkPos()
        val blockPos = BlockPos(chunkPos.startX, 0, chunkPos.startZ)


        // Return the pieces generator that is now set up so that the game runs it when it needs to create the layout of structure pieces.
        return StructurePoolBasedGenerator.generate(
            context,  // Used for StructurePoolBasedGenerator to get all the proper behaviors done.
            this.startPool,
            this.startJigsawName,
            this.size,
            blockPos,
            false,
            this.projectStartToHeightmap,
            this.maxDistanceFromCentre
        )
    }

    override fun getType(): StructureType<*> = VampireHunterModule.SMALL_VAMPIRE_HUNTER_OUTPOST_FEATURE

    companion object {

        val CODEC = RecordCodecBuilder.mapCodec<SmallVampireHunterOutpostStructure> { instance -> instance.group(
            configCodecBuilder(instance),
            StructurePool.REGISTRY_CODEC.fieldOf("start_pool").forGetter(SmallVampireHunterOutpostStructure::startPool),
            Identifier.CODEC.optionalFieldOf("start_jigsaw_name").forGetter(SmallVampireHunterOutpostStructure::startJigsawName),
            Codec.intRange(0, 30).fieldOf("size").forGetter(SmallVampireHunterOutpostStructure::size),
            HeightProvider.CODEC.fieldOf("start_height").forGetter(SmallVampireHunterOutpostStructure::startHeight),
            Heightmap.Type.CODEC.optionalFieldOf("project_start_to_heightmap").forGetter(SmallVampireHunterOutpostStructure::projectStartToHeightmap),
            Codec.intRange(1, 128).fieldOf("max_distance_from_center").forGetter(SmallVampireHunterOutpostStructure::maxDistanceFromCentre)
        ).apply(instance, ::SmallVampireHunterOutpostStructure) }.codec()

        fun canGenerate(ctx: Context): Boolean {
            val centerPos = ctx.chunkPos.getCenterAtY(0)
            val averageHeightAround = (-1..1).asSequence()
                .flatMap { x ->(-1..1).map { z -> x to z } }
                .map { (x, z) -> centerPos.x + x * 16 to centerPos.z + z * 16 }
                .map { (x, z) -> ctx.chunkGenerator.getHeight(x, z, Heightmap.Type.WORLD_SURFACE_WG, ctx.world, ctx.noiseConfig) }
                .average()
            val landHeight = ctx.chunkGenerator.getHeightInGround(centerPos.x, centerPos.z, Heightmap.Type.WORLD_SURFACE_WG, ctx.world, ctx.noiseConfig)

            return ctx.chunkGenerator.getColumnSample(centerPos.x, centerPos.z, ctx.world, ctx.noiseConfig).getState(landHeight).fluidState.isEmpty
                    && 0.3 * ((landHeight - averageHeightAround) + 4) * (landHeight/120.0) > 0.5
        }
    }
}