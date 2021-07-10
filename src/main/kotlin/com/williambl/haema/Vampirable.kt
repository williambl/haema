package com.williambl.haema

import com.williambl.haema.abilities.VampireAbility
import com.williambl.haema.api.AbilityChangeEvent
import com.williambl.haema.api.VampireConversionEvents
import com.williambl.haema.component.VampireComponent
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Identifier

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

    fun getAbilityLevel(ability: VampireAbility): Int = VampireComponent.entityKey.get(this).abilities[ability] ?: 0

    fun setAbilityLevel(ability: VampireAbility, level: Int) {
        AbilityChangeEvent.EVENT.invoker().onAbilityChange(this, ability, level)
        VampireComponent.entityKey.get(this).abilities[ability] = level
        VampireComponent.entityKey.sync(this)
    }

    fun hasUsedRitual(identifier: Identifier) = VampireComponent.entityKey.get(this).ritualsUsed.contains(identifier)

    fun setHasUsedRitual(identifier: Identifier, has: Boolean) {
        val component = VampireComponent.entityKey.get(this)
        if (has)
            component.ritualsUsed.add(identifier)
        else
            component.ritualsUsed.remove(identifier)
        VampireComponent.entityKey.sync(this)
    }

    fun checkBloodManager()
    fun removeBloodManager()

    companion object {
        fun convert(entity: PlayerEntity) {
            if (!(entity as Vampirable).isVampire) {
                entity.isVampire = true
                entity.checkBloodManager()
                entity.health = 1f
                if (entity.world is ServerWorld) {
                    (entity.world as ServerWorld).spawnParticles(DustParticleEffect.DEFAULT, entity.x, entity.y+1, entity.z, 25, 0.5, 1.0, 0.5, 1.0)
                    (entity.world as ServerWorld).spawnParticles(DustParticleEffect(0f, 0f, 0f, 1f), entity.x, entity.y+1, entity.z, 25, 0.5, 1.0, 0.5, 1.0)
                }
                VampireConversionEvents.CONVERT.invoker().onConvert(entity)
            }
        }

        fun deconvert(entity: PlayerEntity) {
            if ((entity as Vampirable).isVampire) {
                entity.isVampire = false
                entity.removeBloodManager()
                VampireConversionEvents.DECONVERT.invoker().onDeconvert(entity)
            }
        }
    }
}
