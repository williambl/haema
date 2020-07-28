package com.williambl.haema.util

import io.github.apace100.origins.Origins
import net.minecraft.block.BedBlock
import net.minecraft.entity.EntityType
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Pair
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Vec3d
import java.util.*

fun getSpawn(player: PlayerEntity, isSpawnObstructed: Boolean): Pair<ServerWorld, BlockPos>? {
    if (player is ServerPlayerEntity) {
        val world = player.world as ServerWorld
        val regularSpawn: BlockPos = world.spawnPos
        val mutable = regularSpawn.mutableCopy()
        var finalPos: Optional<Vec3d> = Optional.empty()
        val range = 128
        var steps = 0
        findspawn@for (dx in -range..range) {
            for (dz in -range..range) {
                yloop@for (dy in 256 downTo -10) {
                    steps++
                    mutable.set(regularSpawn.x+dx, regularSpawn.y+dy, regularSpawn.z+dz)
                    if (world.isSkyVisible(mutable))
                        continue@yloop
                    finalPos = BedBlock.canWakeUpAt(EntityType.PLAYER, world, mutable)
                    if (finalPos.isPresent)
                        break@findspawn
                }
            }
        }
        return if (finalPos.isPresent) {
            Origins.LOGGER.info("Found a spawn, ${finalPos.get().distanceTo(Vec3d.ofCenter(regularSpawn))} blocks from regular spawn, in $steps steps.")
            Pair(world, BlockPos(finalPos.get()))
        } else {
            Origins.LOGGER.warn("Could not find spawn for player with Vampirism in the nether in range $range of $regularSpawn.")
            null
        }
    }
    return null
}