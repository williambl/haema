package com.williambl.haema.util

import net.minecraft.entity.player.PlayerEntity
import net.minecraft.util.hit.BlockHitResult
import net.minecraft.util.hit.HitResult
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Direction
import net.minecraft.util.math.Vec3d
import net.minecraft.world.RaycastContext
import net.minecraft.world.World

fun raytraceForDash(player: PlayerEntity): Vec3d? {
    val world = player.world

    val eyeVec: Vec3d = player.getCameraPosVec(0f)
    val dir: Vec3d = player.getRotationVec(0f)
    val rayEnd = eyeVec.add(dir.x * 16, dir.y * 16, dir.z * 16)
    val result: BlockHitResult = world.raycast(RaycastContext(eyeVec, rayEnd, RaycastContext.ShapeType.COLLIDER, RaycastContext.FluidHandling.NONE, player))

    var dashPos = when (result.side) {
        Direction.DOWN -> result.blockPos.down(2)
        else -> result.blockPos.offset(result.side)
    }

    var posIsFree = posIsClear(world, dashPos)
    while (!posIsFree) {
        dashPos = dashPos.down()
        posIsFree = posIsClear(world, dashPos) && world.raycast(RaycastContext(eyeVec, Vec3d.ofCenter(dashPos.up()), RaycastContext.ShapeType.COLLIDER, RaycastContext.FluidHandling.NONE, player)).type == HitResult.Type.MISS
        if (dashPos.y <= 0)
            break
    }
    return if (posIsFree) Vec3d.ofCenter(dashPos) else null
}

fun posIsClear(world: World, pos: BlockPos): Boolean {
    return (world.isAir(pos) || world.getBlockState(pos).getCollisionShape(world, pos).isEmpty)
            && (world.isAir(pos.up()) || world.getBlockState(pos.up()).getCollisionShape(world, pos.up()).isEmpty)
}
