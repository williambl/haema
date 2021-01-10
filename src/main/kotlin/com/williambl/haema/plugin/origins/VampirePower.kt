package com.williambl.haema.plugin.origins

import com.williambl.haema.Vampirable
import com.williambl.haema.logger
import com.williambl.haema.util.getSpawn
import io.github.apace100.origins.power.Power
import io.github.apace100.origins.power.PowerType
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundTag
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Identifier
import net.minecraft.util.Pair
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Vec3d
import net.minecraft.util.registry.Registry

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
        val bookStack = ItemStack(Registry.ITEM[Identifier("patchouli:guide_book")])
        val tag = CompoundTag()
        tag.putString("patchouli:book", "haema:book_of_blood")
        bookStack.tag = tag
        player.inventory.offerOrDrop(player.world, bookStack)

        if (isOrbOfOrigin) return
        if (player is ServerPlayerEntity) {
            val serverPlayer = player as ServerPlayerEntity
            val spawn: Pair<ServerWorld, BlockPos>? = getSpawn(player)
            if (!isOrbOfOrigin) {
                if (spawn != null) {
                    val tpPos = Vec3d.ofCenter(spawn.right)
                    serverPlayer.teleport(
                        spawn.left,
                        tpPos.x,
                        tpPos.y,
                        tpPos.z,
                        player.pitch,
                        player.yaw
                    )
                    serverPlayer.setSpawnPoint(spawn.left.registryKey, spawn.right, 0.0f, true, false)

                } else {
                    logger.warn("Could not spawn Vampire player away from sunlight.")
                }
                if (serverPlayer.server.currentPlayerCount == 1) {
                    serverPlayer.serverWorld.timeOfDay = 13000
                }
            } else {
                logger.warn("Could not spawn Vampire player away from sunlight.")
            }
        }
    }
}
