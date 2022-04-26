package com.williambl.haema.spells

import net.minecraft.entity.EntityType
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.passive.BatEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.particle.DustParticleEffect
import net.minecraft.particle.ParticleEffect
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ChunkTicketType
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundEvents
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.ChunkPos
import net.minecraft.util.math.Vec3d
import net.minecraft.util.math.Vec3f
import net.minecraft.util.registry.RegistryKey
import net.minecraft.world.World

class DissolutionSpell: Spell() {
    override val chargeTime: Int = 60

    override fun chargeParticle(user: LivingEntity): ParticleEffect = DustParticleEffect(Vec3f(0f, 0f, 0f), 1f)

    override fun use(world: World, user: LivingEntity) {
        for (i in 0..50) {
            world.addParticle(
                DustParticleEffect(Vec3f(0f, 0f, 0f), 1f),
                user.x + world.random.nextGaussian() * user.width/2.0,
                user.randomBodyY,
                user.z + world.random.nextGaussian() * user.width/2.0,
                0.0,
                0.5,
                0.0
            )
        }

        if (world is ServerWorld) {
            for (i in 0..9) {
                world.spawnEntity(BatEntity(EntityType.BAT, world).also {
                    it.setPos(
                        user.x + world.random.nextGaussian() * user.width/2.0,
                        user.randomBodyY,
                        user.z + world.random.nextGaussian() * user.width/2.0
                    )
                    it.setVelocity(0.0, 0.15, 0.0)
                    it.playSound(SoundEvents.ENTITY_BAT_TAKEOFF, 1.0f, 0.8f+world.random.nextGaussian().toFloat()*0.3f)
                })
            }

            val spawnWorld = world.server.getWorld(user.spawnWorld())
            if (spawnWorld !is ServerWorld) {
                return
            }

            val spawnPos = user.spawnPos()?.let { pos ->
                PlayerEntity.findRespawnPosition(spawnWorld, pos, user.spawnAngle(), false, true)
                    .map { Pair(it, user.spawnAngle()) }
                    .get()
            } ?: Pair(Vec3d.ofBottomCenter(spawnWorld.spawnPos), spawnWorld.spawnAngle)

            spawnWorld.chunkManager.addTicket(ChunkTicketType.POST_TELEPORT, ChunkPos(BlockPos(spawnPos.first)), 1, user.id)
            user.teleport(spawnWorld, spawnPos.first.x, spawnPos.first.y, spawnPos.first.z, spawnPos.second, 0.0f)
        }
    }

    private fun LivingEntity.spawnPos(): BlockPos? =
        if (this is ServerPlayerEntity) {
            this.spawnPointPosition
        } else {
            null
        }

    private fun LivingEntity.spawnAngle(): Float =
        if (this is ServerPlayerEntity) {
            this.spawnAngle
        } else {
            0f
        }

    private fun LivingEntity.spawnWorld(): RegistryKey<World> =
        if (this is ServerPlayerEntity) {
            this.spawnPointDimension
        } else {
            this.world.registryKey
        }

    private fun LivingEntity.teleport(spawnWorld: ServerWorld, x: Double, y: Double, z: Double, yaw: Float, pitch: Float) {
        if (this is ServerPlayerEntity) {
            this.teleport(spawnWorld, x, y, z, yaw, pitch)
        } else {
            this.teleport(x, y, z)
        }
    }

}