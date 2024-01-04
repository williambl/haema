package com.williambl.haema.compat.origins

import com.williambl.haema.Haema
import com.williambl.haema.criteria.VampireConversionCriterion
import com.williambl.haema.isPermanentVampire
import com.williambl.haema.isVampirable
import com.williambl.haema.isVampire
import com.williambl.haema.util.getSpawn
import io.github.apace100.apoli.power.ModifyPlayerSpawnPower
import io.github.apace100.apoli.power.PowerType
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.registry.Registries
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Identifier
import net.minecraft.util.Pair
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Vec3d


class VampirePower(type: PowerType<*>?, entity: LivingEntity) : ModifyPlayerSpawnPower(type, entity, null, 1.0f, null, null, null, null) {
    override fun onAdded() {
        if (entity.isVampirable()) {
            (entity).isVampire = true
            (entity).isPermanentVampire = true
        }
    }

    override fun onLost() {
        (entity).isVampire = false
        (entity).isPermanentVampire = false
    }

    override fun getSpawn(isSpawnObstructed: Boolean): Pair<ServerWorld, BlockPos>? {
        if (entity is PlayerEntity && entity.world != null) {
            return getSpawn(entity as PlayerEntity)
        }
        return null
    }

    override fun teleportToModifiedSpawn() {
        if (entity is ServerPlayerEntity) {
            val serverPlayer = entity as ServerPlayerEntity
            val spawn: Pair<ServerWorld, BlockPos>? = getSpawn(entity as ServerPlayerEntity)

            if (spawn != null) {
                val tpPos = Vec3d.ofCenter(spawn.right)
                serverPlayer.teleport(
                    spawn.left,
                    tpPos.x,
                    tpPos.y,
                    tpPos.z,
                    entity.pitch,
                    entity.yaw
                )
                serverPlayer.setSpawnPoint(spawn.left.registryKey, spawn.right, 0.0f, true, false)

            } else {
                Haema.LOGGER.warn("Could not spawn Vampire entity away from sunlight.")
            }
            if (serverPlayer.server.currentPlayerCount == 1) {
                serverPlayer.serverWorld.timeOfDay = 13000
            }
        }
    }

    override fun onGained() {
        if (entity is ServerPlayerEntity) {
            VampireConversionCriterion.trigger(entity as ServerPlayerEntity)
        }
        if (entity is PlayerEntity) {
            val bookStack = ItemStack(Registries.ITEM[Identifier("haema:book_of_blood")])
            (entity as PlayerEntity).inventory.offerOrDrop(bookStack)
        }
    }
}
