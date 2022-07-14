package com.williambl.haema.vampiremobs.elder_vampire

import net.minecraft.block.Block
import net.minecraft.block.BlockState
import net.minecraft.block.ShapeContext
import net.minecraft.util.math.BlockPos
import net.minecraft.util.shape.VoxelShape
import net.minecraft.util.shape.VoxelShapes
import net.minecraft.world.BlockView

class VampiricEssenceBlock(settings: Settings) : Block(settings) {
    override fun getOpacity(state: BlockState?, world: BlockView, pos: BlockPos?): Int {
        return world.maxLightLevel
    }

    override fun getCameraCollisionShape(
        state: BlockState?,
        world: BlockView?,
        pos: BlockPos?,
        context: ShapeContext?
    ): VoxelShape? {
        return VoxelShapes.empty()
    }

    override fun getAmbientOcclusionLightLevel(state: BlockState?, world: BlockView?, pos: BlockPos?): Float {
        return 1.0f
    }

}