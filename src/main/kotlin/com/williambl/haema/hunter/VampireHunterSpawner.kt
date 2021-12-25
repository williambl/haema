package com.williambl.haema.hunter

import com.williambl.haema.isVampire
import net.minecraft.entity.EntityData
import net.minecraft.entity.EntityType
import net.minecraft.entity.SpawnReason
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.mob.PatrolEntity
import net.minecraft.entity.passive.PassiveEntity
import net.minecraft.nbt.NbtCompound
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.math.BlockPos
import net.minecraft.world.GameRules
import net.minecraft.world.Heightmap
import net.minecraft.world.SpawnHelper
import net.minecraft.world.biome.Biome
import net.minecraft.world.gen.Spawner
import java.util.*
import kotlin.math.ceil

class VampireHunterSpawner: Spawner {
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

        val vampires = serverWorld.players.filter(ServerPlayerEntity::isVampire)

        val vampireCount = vampires.size
        if (vampireCount < 1)
            return 0

        val player = vampires[random.nextInt(vampireCount)]

        if (player.isSpectator)
            return 0
        if (serverWorld.isNearOccupiedPointOfInterest(player.blockPos, 2))
            return 0

        return trySpawnNear(serverWorld, random, player.blockPos)
    }

    fun trySpawnNear(serverWorld: ServerWorld, random: Random, pos: BlockPos): Int {
        amountSpawnedSinceLast = 0
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

        val spawnFunction = if (random.nextDouble() < 0.1) ::spawnOneEntityOnHorse else ::spawnOneEntity

        for (i in 0 until localDifficulty) {
            ++amountToSpawn
            mutable.y = serverWorld.getTopPosition(Heightmap.Type.MOTION_BLOCKING_NO_LEAVES, mutable).y
            if (i == 0) {
                if (!spawnFunction.call(serverWorld, mutable, random, true)) {
                    break
                }
            } else {
                spawnFunction.call(serverWorld, mutable, random, false)
            }
            mutable.x = mutable.x + random.nextInt(5) - random.nextInt(5)
            mutable.z = mutable.z + random.nextInt(5) - random.nextInt(5)
        }
        return amountToSpawn
    }

    fun spawnOneEntity(world: ServerWorld, blockPos: BlockPos, random: Random, isLeader: Boolean): Boolean {
        val blockState = world.getBlockState(blockPos)
        if (!SpawnHelper.isClearForSpawn(world, blockPos, blockState, blockState.fluidState, VampireHunterModule.VAMPIRE_HUNTER))
            return false
        if (!PatrolEntity.canSpawn(VampireHunterModule.VAMPIRE_HUNTER, world, SpawnReason.PATROL, blockPos, random))
            return false
        val entity = VampireHunterModule.VAMPIRE_HUNTER.create(world)
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
                null as NbtCompound?
            )
            world.spawnEntity(entity)
            return true
        }
        return false
    }

    fun spawnOneEntityOnHorse(world: ServerWorld, blockPos: BlockPos, random: Random, isLeader: Boolean): Boolean {
        val blockState = world.getBlockState(blockPos)
        if (!SpawnHelper.isClearForSpawn(world, blockPos, blockState, blockState.fluidState, VampireHunterModule.VAMPIRE_HUNTER))
            return false
        if (!PatrolEntity.canSpawn(VampireHunterModule.VAMPIRE_HUNTER, world, SpawnReason.PATROL, blockPos, random))
            return false
        val hunter = VampireHunterModule.VAMPIRE_HUNTER.create(world) ?: return false
        val horse = EntityType.HORSE.create(world) ?: return false
        amountSpawnedSinceLast++

        horse.initialize(world, world.getLocalDifficulty(blockPos), SpawnReason.PATROL, PassiveEntity.PassiveData(false), null)
        horse.isTame = true
        //horse.saddle(null)
        horse.timeUntilRegen = 60
        horse.updatePosition(
            blockPos.x.toDouble(),
            blockPos.y.toDouble(),
            blockPos.z.toDouble()
        )
        horse.getAttributeInstance(EntityAttributes.GENERIC_MOVEMENT_SPEED)?.let {
            it.baseValue += 0.2
        }

        if (isLeader) {
            hunter.isPatrolLeader = true
            hunter.setRandomPatrolTarget()
        }
        hunter.updatePosition(
            blockPos.x.toDouble(),
            blockPos.y.toDouble(),
            blockPos.z.toDouble()
        )
        hunter.initialize(
            world,
            world.getLocalDifficulty(blockPos),
            SpawnReason.PATROL,
            null as EntityData?,
            null as NbtCompound?
        )
        hunter.startRiding(horse)
        world.spawnEntityAndPassengers(horse)
        return true
    }

    companion object {
        val instance = VampireHunterSpawner()
    }
}
