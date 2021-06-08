package com.williambl.haema.compat.origins

import com.williambl.haema.Vampirable
import com.williambl.haema.criteria.VampireConversionCriterion
import com.williambl.haema.logger
import com.williambl.haema.util.getSpawn
import io.github.apace100.origins.power.Power
import io.github.apace100.origins.power.PowerType
import net.fabricmc.loader.api.FabricLoader
import net.fabricmc.loader.api.SemanticVersion
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NbtCompound
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
        val originsVersion = FabricLoader.getInstance().getModContainer("origins").get().metadata.version
        if (originsVersion is SemanticVersion
                && (
                        originsVersion < SemanticVersion.parse("0.4.6")
                        || ((originsVersion == SemanticVersion.parse("0.4.6") || originsVersion == SemanticVersion.parse("0.4.7")) && Throwable().stackTrace[1].className.contains("PlayerOriginComponent"))) // awful hack I know
                    ) {
            (player as Vampirable).isVampire = false
            (player as Vampirable).isPermanentVampire = false
            (player as Vampirable).removeBloodManager()
        }
    }

    override fun onLost() {
        (player as Vampirable).isVampire = false
        (player as Vampirable).isPermanentVampire = false
        (player as Vampirable).removeBloodManager()
    }

    override fun onChosen(isOrbOfOrigin: Boolean) {
        if (player is ServerPlayerEntity) {
            VampireConversionCriterion.trigger(player as ServerPlayerEntity)
        }
        val bookStack = ItemStack(Registry.ITEM[Identifier("patchouli:guide_book")])
        val tag = NbtCompound()
        tag.putString("patchouli:book", "haema:book_of_blood")
        bookStack.tag = tag
        player.inventory.offerOrDrop(bookStack)

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
