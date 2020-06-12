package com.williambl.haema

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.damage.DamageSource
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.entity.effect.StatusEffectType
import net.minecraft.entity.player.PlayerEntity

class SunlightSicknessEffect(type: StatusEffectType?, color: Int) : StatusEffect(type, color) {

    companion object {
        lateinit var instance: StatusEffect
    }

    override fun applyUpdateEffect(entity: LivingEntity?, amplifier: Int) {
        if (entity !is PlayerEntity)
            return

        if (entity.getHealth() > 1.0f) {
            entity.damage(DamageSource.MAGIC, 1.0f)
        }
    }

    override fun canApplyUpdateEffect(duration: Int, amplifier: Int): Boolean {
        val k = 25 shr amplifier
        return if (k > 0) {
            duration % k == 0
        } else {
            true
        }
    }
}