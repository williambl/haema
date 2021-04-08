package com.williambl.haema.hunter

import com.williambl.haema.Vampirable
import net.minecraft.entity.EntityData
import net.minecraft.entity.EntityType
import net.minecraft.entity.SpawnReason
import net.minecraft.entity.mob.PatrolEntity
import net.minecraft.nbt.CompoundTag
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Identifier
import net.minecraft.util.math.BlockPos
import net.minecraft.util.registry.Registry
import net.minecraft.world.GameRules
import net.minecraft.world.Heightmap
import net.minecraft.world.SpawnHelper
import net.minecraft.world.biome.Biome
import net.minecraft.world.gen.Spawner
import java.util.*
import kotlin.math.ceil

class VampireHunterSpawner(private val entityType: EntityType<out VampireHunterEntity>) : Spawner {
    private var ticksUntilNextSpawn = 0

    private var amountSpawnedSinceLast = 0

    override fun spawn(serverWorld: ServerWorld, spawnMonsters: Boolean, spawnAnimals: Boolean): Int {
        if (!spawnMonsters)
            return 0
        if (!serverWorld.gameRules.getBoolean(GameRules.DO_PATROL_SPAWNING))
            return 0

        val random = serverWorld.random
        --ticksUntilNextSpawn
        if (ticksUntilNextSpawn > 0)
            return 0

        ticksUntilNextSpawn += 12000 + random.nextInt(1200)
        amountSpawnedSinceLast = 0

        if (random.nextInt(5) != 0)
            return 0

        val vampires = serverWorld.players.filter { (it as Vampirable).isVampire }

        val vampireCount = vampires.size
        if (vampireCount < 1)
            return 0

        val player = vampires[random.nextInt(vampireCount)]

        if (player.isSpectator)
            return 0
        if (serverWorld.isNearOccupiedPointOfInterest(player.blockPos, 2))
            return 0

        val amountSpawned = trySpawnNear(serverWorld, random, player.blockPos)
        println("spawned $amountSpawned")
        return amountSpawned
    }

    fun trySpawnNear(serverWorld: ServerWorld, random: Random, pos: BlockPos): Int {
        if (amountSpawnedSinceLast > 10)
            return 0

        val dx = (24 + random.nextInt(24)) * if (random.nextBoolean()) -1 else 1
        val dz = (24 + random.nextInt(24)) * if (random.nextBoolean()) -1 else 1
        val mutable = pos.mutableCopy().move(dx, 0, dz)

        if (!serverWorld.isRegionLoaded(mutable.x - 10, mutable.y - 10, mutable.z - 10, mutable.x + 10, mutable.y + 10, mutable.z + 10))
            return 0

        val biome = serverWorld.getBiome(mutable)
        val category = biome.category
        if (category == Biome.Category.MUSHROOM)
            return 0

        var amountToSpawn = 0
        val localDifficulty = ceil(serverWorld.getLocalDifficulty(mutable).localDifficulty.toDouble()).toInt() + 1

        for (i in 0 until localDifficulty) {
            ++amountToSpawn
            mutable.y = serverWorld.getTopPosition(Heightmap.Type.MOTION_BLOCKING_NO_LEAVES, mutable).y
            if (i == 0) {
                if (!spawnOneEntity(serverWorld, mutable, random, true)) {
                    break
                }
            } else {
                spawnOneEntity(serverWorld, mutable, random, false)
            }
            mutable.x = mutable.x + random.nextInt(5) - random.nextInt(5)
            mutable.z = mutable.z + random.nextInt(5) - random.nextInt(5)
        }
        return amountToSpawn
    }

    private fun spawnOneEntity(world: ServerWorld, blockPos: BlockPos, random: Random, isLeader: Boolean): Boolean {
        val blockState = world.getBlockState(blockPos)
        if (!SpawnHelper.isClearForSpawn(world, blockPos, blockState, blockState.fluidState, entityType))
            return false
        if (!PatrolEntity.canSpawn(entityType, world, SpawnReason.PATROL, blockPos, random))
            return false
        val entity = entityType.create(world)
        if (entity != null) {
            amountSpawnedSinceLast++
            if (isLeader) {
                entity.isPatrolLeader = true
                entity.setRandomPatrolTarget()
            }
            entity.updatePosition(
                blockPos.x.toDouble(),
                blockPos.y.toDouble(),
                blockPos.z.toDouble()
            )
            entity.initialize(
                world,
                world.getLocalDifficulty(blockPos),
                SpawnReason.PATROL,
                null as EntityData?,
                null as CompoundTag?
            )
            world.spawnEntity(entity)
            return true
        }
        return false
    }

    companion object {
        @Suppress("UNCHECKED_CAST")
        val instance: VampireHunterSpawner by lazy { VampireHunterSpawner(Registry.ENTITY_TYPE.get(Identifier("haema:vampire_hunter")) as EntityType<out VampireHunterEntity>) }
    }
}
