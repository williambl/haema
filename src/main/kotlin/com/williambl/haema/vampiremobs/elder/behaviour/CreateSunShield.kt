package com.williambl.haema.vampiremobs.elder.behaviour

import com.mojang.datafixers.util.Pair
import com.williambl.haema.vampiremobs.VampireMobsModule
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.brain.MemoryModuleState
import net.minecraft.entity.ai.brain.MemoryModuleType
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour
import net.tslat.smartbrainlib.util.BrainUtils

class CreateSunShield<E: LivingEntity>(val cooldown: Int) : ExtendedBehaviour<E>() {
    override fun getMemoryRequirements(): List<Pair<MemoryModuleType<*>, MemoryModuleState>> = listOf(
        Pair(VampireMobsModule.SKY_VISIBLE_MEMORY, MemoryModuleState.VALUE_PRESENT),
        Pair(VampireMobsModule.SUN_SHIELD_COOLDOWN_MEMORY, MemoryModuleState.VALUE_ABSENT)
    )

    override fun start(entity: E) {
        val sunShieldProjectile = VampireMobsModule.SUN_SHIELD_PROJECTILE.create(entity.world) ?: return
        sunShieldProjectile.owner = entity
        sunShieldProjectile.updatePosition(entity.x, entity.y + entity.eyeY, entity.z)
        sunShieldProjectile.setVelocity(0.0, 0.2, 0.0)
        entity.world.spawnEntity(sunShieldProjectile)
        BrainUtils.setForgettableMemory(entity, VampireMobsModule.SUN_SHIELD_COOLDOWN_MEMORY, com.mojang.datafixers.util.Unit.INSTANCE, cooldown)
    }
}