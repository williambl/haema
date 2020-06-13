package com.williambl.haema.effect

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.damage.DamageSource
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.entity.effect.StatusEffectType
import net.minecraft.entity.player.PlayerEntity

class VampiricStrengthEffect(type: StatusEffectType?, color: Int) : StatusEffect(type, color) {

    companion object {
        lateinit var instance: StatusEffect
    }

    override fun applyUpdateEffect(entity: LivingEntity?, amplifier: Int) {}

    override fun canApplyUpdateEffect(duration: Int, amplifier: Int): Boolean { return false }
}