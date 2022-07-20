package com.williambl.haema.hunter.structure

/*
class VampireHunterOutpostStructure(
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
    companion object {
        val CODEC = RecordCodecBuilder.mapCodec<VampireHunterOutpostStructure> { instance -> instance.group(
            configCodecBuilder(instance),
            StructurePool.REGISTRY_CODEC.fieldOf("start_pool").forGetter(VampireHunterOutpostStructure::startPool),
            Identifier.CODEC.optionalFieldOf("start_jigsaw_name").forGetter(VampireHunterOutpostStructure::startJigsawName),
            Codec.intRange(0, 30).fieldOf("size").forGetter(VampireHunterOutpostStructure::size),
            HeightProvider.CODEC.fieldOf("start_height").forGetter(VampireHunterOutpostStructure::startHeight),
            Heightmap.Type.CODEC.optionalFieldOf("project_start_to_heightmap").forGetter(VampireHunterOutpostStructure::projectStartToHeightmap),
            Codec.intRange(1, 128).fieldOf("max_distance_from_center").forGetter(VampireHunterOutpostStructure::maxDistanceFromCentre)
        ).apply(instance, ::VampireHunterOutpostStructure) }.codec()

        fun canGenerate(ctx: Context): Boolean {
            val centerPos = BlockPos(ctx.chunkPos.centerX, 0, ctx.chunkPos.centerZ)
            val landHeight = ctx.chunkGenerator.getHeightInGround(
                centerPos.x,
                centerPos.z,
                Heightmap.Type.WORLD_SURFACE_WG,
                ctx.world,
                ctx.noiseConfig
            )
            return ctx.chunkGenerator.getColumnSample(centerPos.x, centerPos.z, ctx.world, ctx.noiseConfig).getState(landHeight).fluidState.isEmpty
        }
    }

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

    override fun getType(): StructureType<*> = VampireHunterModule.VAMPIRE_HUNTER_OUTPOST_FEATURE
}

*/