package com.williambl.haema.plugin.origins

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.logger
import com.williambl.haema.util.getSpawn
import io.github.apace100.origins.power.Power
import io.github.apace100.origins.power.PowerType
import net.minecraft.block.BedBlock
import net.minecraft.entity.EntityType
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Pair
import net.minecraft.util.math.BlockPos

class VampirePower(type: PowerType<*>?, player: PlayerEntity?) : Power(type, player) {
    override fun onAdded() {
        (player as Vampirable).isVampire = true
        (player as Vampirable).isPermanentVampire = true
    }

    override fun onRemoved() {
        (player as Vampirable).isVampire = false
        (player as Vampirable).isPermanentVampire = false
    }

    override fun onChosen(isOrbOfOrigin: Boolean) {
        (player.hungerManager as VampireBloodManager).addBlood(8.0)
        if (isOrbOfOrigin) return
        if (player is ServerPlayerEntity) {
            val serverPlayer = player as ServerPlayerEntity
            val spawn: Pair<ServerWorld, BlockPos>? = getSpawn(player, false)
            if (!isOrbOfOrigin) {
                if (spawn != null) {
                    val tpPos =
                        BedBlock.canWakeUpAt(EntityType.PLAYER, spawn.left, spawn.right)
                    if (tpPos.isPresent) {
                        serverPlayer.teleport(
                            spawn.left,
                            tpPos.get().x,
                            tpPos.get().y,
                            tpPos.get().z,
                            player.pitch,
                            player.yaw
                        )
                    } else {
                        logger.warn("Could not spawn Vampire player with away from sunlight.")
                    }
                } else {
                    logger.warn("Could not spawn Vampire player away from sunlight.")
                }
            }
        }
    }
}