package com.williambl.haema.effect

import com.williambl.haema.damagesource.SunlightDamageSource
import com.williambl.haema.isVampire
import com.williambl.haema.vampireComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.entity.effect.StatusEffectCategory
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.particle.ParticleTypes
import net.minecraft.server.world.ServerWorld

class SunlightSicknessEffect(type: StatusEffectCategory?, color: Int) : StatusEffect(type, color) {

    companion object {
        val instance: StatusEffect = SunlightSicknessEffect(StatusEffectCategory.HARMFUL, 245 shl 24 or 167 shl 16 or 66 shl 8)
                .addAttributeModifier(
                        EntityAttributes.GENERIC_ATTACK_DAMAGE,
                        "c85d1cfe-2c10-4d25-b650-49c045979842",
                        -4.0,
                        EntityAttributeModifier.Operation.ADDITION
                )
    }

    override fun applyUpdateEffect(entity: LivingEntity?, amplifier: Int) {
        if (entity == null || !entity.isVampire || entity.isSpectator || (entity is PlayerEntity && entity.isCreative)) {
            return
        }

        if (entity.age % 10 == 0) {
            entity.damage(SunlightDamageSource.instance, 0.5f)
            if (entity.isVampire) {
                (entity.vampireComponent).removeBlood(0.25)
            }
            val pos = entity.pos
            val world = entity.world
            if (world is ServerWorld) {
                world.spawnParticles(
                    ParticleTypes.FLAME,
                    pos.x - entity.width/4.0,
                    pos.y,
                    pos.z - entity.width/4.0,
                    10,
                    entity.width.toDouble()/2.0,
                    entity.height.toDouble()/2.0,
                    entity.width.toDouble()/2.0,
                    0.0
                )
            }
        }
    }

    override fun canApplyUpdateEffect(duration: Int, amplifier: Int): Boolean {
        return true
    }
}