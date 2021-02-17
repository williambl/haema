package com.williambl.haema

import com.williambl.haema.component.VampireComponent
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.world.ServerWorld

interface Vampirable {

    var isVampire: Boolean
        get() = VampireComponent.entityKey.get(this).isVampire
        set(value) {
            VampireComponent.entityKey.get(this).isVampire = value
        }

    var isPermanentVampire: Boolean
        get() = VampireComponent.entityKey.get(this).isPermanentVampire
        set(value) {
            VampireComponent.entityKey.get(this).isPermanentVampire = value
        }

    var isKilled: Boolean
        get() = VampireComponent.entityKey.get(this).isKilled
        set(value) {
            VampireComponent.entityKey.get(this).isKilled = value
        }

    fun checkBloodManager();
    fun removeBloodManager();

    companion object {
        fun convert(entity: PlayerEntity) {
            if (!(entity as Vampirable).isVampire) {
                entity.isVampire = true
                entity.checkBloodManager()
                entity.health = 1f
                if (entity.world is ServerWorld) {
                    (entity.world as ServerWorld).spawnParticles(DustParticleEffect.RED, entity.x, entity.y+1, entity.z, 25, 0.5, 1.0, 0.5, 1.0)
                    (entity.world as ServerWorld).spawnParticles(DustParticleEffect(0f, 0f, 0f, 1f), entity.x, entity.y+1, entity.z, 25, 0.5, 1.0, 0.5, 1.0)
                }
            }
        }
    }
}
