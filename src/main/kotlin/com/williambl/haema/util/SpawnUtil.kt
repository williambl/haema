package com.williambl.haema.util

import com.williambl.haema.logger
import net.minecraft.block.BedBlock
import net.minecraft.entity.EntityType
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.fluid.Fluids
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Pair
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Vec3d

fun getSpawn(player: PlayerEntity, isSpawnObstructed: Boolean): Pair<ServerWorld, BlockPos>? {
    if (player is ServerPlayerEntity) {
        val world = player.world as ServerWorld
        val regularSpawn: BlockPos = world.spawnPos
        val mutable = regularSpawn.mutableCopy()
        var candidates = mutableListOf<Vec3d>()
        val range = 128
        findspawn@for (dx in -range..range) {
            for (dz in -range..range) {
                yloop@for (dy in 256 downTo -10) {
                    mutable.set(regularSpawn.x+dx, regularSpawn.y+dy, regularSpawn.z+dz)
                    if (world.isSkyVisible(mutable))
                        continue@yloop
                    if (world.getFluidState(mutable).fluid != Fluids.EMPTY)
                        continue@yloop
                    val candidate = BedBlock.canWakeUpAt(EntityType.PLAYER, world, mutable)
                    if (candidate.isPresent)
                        candidates.add(candidate.get())
                }
            }
        }
        val avgY = candidates.map { it.y }.average()
        candidates = candidates.sortedBy { it.distanceTo(Vec3d.ofCenter(regularSpawn)) }.filter { it.y >= avgY }.toMutableList()
        return if (candidates.isNotEmpty()) {
            logger.info("Found a vampire-safe spawn, ${candidates.first().distanceTo(Vec3d.ofCenter(regularSpawn))} blocks from regular spawn.")
            Pair(world, BlockPos(candidates.first()))
        } else {
            logger.warn("Could not find a vampire-safe spawn in range $range of $regularSpawn.")
            null
        }
    }
    return null
}
