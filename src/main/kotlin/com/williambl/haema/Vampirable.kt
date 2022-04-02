package com.williambl.haema

import com.williambl.haema.ability.VampireAbility
import com.williambl.haema.api.AbilityChangeEvent
import com.williambl.haema.api.VampireConversionEvents
import com.williambl.haema.component.VampireComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Identifier
import net.minecraft.util.math.Vec3f

val LivingEntity.vampireComponent: VampireComponent
    get() = VampireComponent.entityKey.get(this)

var LivingEntity.isVampire: Boolean
    get() = VampireComponent.entityKey.getNullable(this)?.isVampire ?: false
    set(value) {
        VampireComponent.entityKey.getNullable(this)?.isVampire = value
    }

var LivingEntity.isPermanentVampire: Boolean
    get() = VampireComponent.entityKey.getNullable(this)?.isPermanentVampire ?: false
    set(value) {
        VampireComponent.entityKey.getNullable(this)?.isPermanentVampire = value
    }

var LivingEntity.isKilled: Boolean
    get() = VampireComponent.entityKey.getNullable(this)?.isKilled ?: false
    set(value) {
        VampireComponent.entityKey.getNullable(this)?.isKilled = value
    }

fun LivingEntity.isVampirable(): Boolean = VampireComponent.entityKey.isProvidedBy(this)

fun LivingEntity.getAbilityLevel(ability: VampireAbility): Int = VampireComponent.entityKey.getNullable(this)?.abilities?.get(ability) ?: 0

fun LivingEntity.setAbilityLevel(ability: VampireAbility, level: Int) {
    if (!this.isVampirable()) {
        return
    }
    AbilityChangeEvent.EVENT.invoker().onAbilityChange(this, ability, level)
    VampireComponent.entityKey.get(this).abilities[ability] = level
    VampireComponent.entityKey.sync(this)
}

fun LivingEntity.hasUsedRitual(identifier: Identifier) = VampireComponent.entityKey.getNullable(this)?.ritualsUsed?.contains(identifier) ?: false

fun LivingEntity.setHasUsedRitual(identifier: Identifier, has: Boolean) {
    val component = VampireComponent.entityKey.getNullable(this) ?: return
    if (has)
        component.ritualsUsed.add(identifier)
    else
        component.ritualsUsed.remove(identifier)
    VampireComponent.entityKey.sync(this)
}

fun convert(entity: LivingEntity) {
    if (!entity.isVampire && entity.isVampirable()) {
        entity.isVampire = true
        entity.health = 1f
        if (entity.world is ServerWorld) {
            (entity.world as ServerWorld).spawnParticles(DustParticleEffect.DEFAULT, entity.x, entity.y+1, entity.z, 25, 0.5, 1.0, 0.5, 1.0)
            (entity.world as ServerWorld).spawnParticles(DustParticleEffect(Vec3f(0f, 0f, 0f), 1f), entity.x, entity.y+1, entity.z, 25, 0.5, 1.0, 0.5, 1.0)
        }
        VampireConversionEvents.CONVERT.invoker().onConvert(entity)
    }
}

fun deconvert(entity: LivingEntity) {
    if (entity.isVampire && entity.isVampirable()) {
        entity.isVampire = false
        VampireConversionEvents.DECONVERT.invoker().onDeconvert(entity)
    }
}
