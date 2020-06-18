package com.williambl.haema

import net.minecraft.entity.data.DataTracker
import net.minecraft.entity.data.TrackedData
import net.minecraft.entity.data.TrackedDataHandlerRegistry
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.world.ServerWorld

interface Vampirable {
    var isVampire: Boolean

    fun checkBloodManager();

    companion object {

        val IS_VAMPIRE: TrackedData<Boolean> = DataTracker.registerData(PlayerEntity::class.java, TrackedDataHandlerRegistry.BOOLEAN)
        val IS_KILLED: TrackedData<Boolean> = DataTracker.registerData(PlayerEntity::class.java, TrackedDataHandlerRegistry.BOOLEAN)

        fun convert(entity: PlayerEntity) {
            if (!(entity as Vampirable).isVampire) {
                entity.isVampire = true
                (entity.hungerManager as VampireBloodManager).absoluteBloodLevel = 3.0
                entity.health = 1f
                if (entity.world is ServerWorld) {
                    (entity.world as ServerWorld).spawnParticles(DustParticleEffect.RED, entity.x, entity.y+1, entity.z, 25, 0.5, 1.0, 0.5, 1.0)
                    (entity.world as ServerWorld).spawnParticles(DustParticleEffect(0f, 0f, 0f, 1f), entity.x, entity.y+1, entity.z, 25, 0.5, 1.0, 0.5, 1.0)
                }
            }
        }
    }
}
