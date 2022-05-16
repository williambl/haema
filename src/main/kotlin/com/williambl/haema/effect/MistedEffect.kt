package com.williambl.haema.effect

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.damage.DamageSource
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.entity.effect.StatusEffectCategory

class MistedEffect(type: StatusEffectCategory?, color: Int) : StatusEffect(type, color) {
    companion object {
        val instance: StatusEffect = MistedEffect(StatusEffectCategory.HARMFUL, 0x89bfa9)
            .addAttributeModifier(EntityAttributes.GENERIC_MOVEMENT_SPEED, "fc24f0fa-1f5e-476f-b1da-046be2ce382d", -0.5, EntityAttributeModifier.Operation.MULTIPLY_BASE)
    }

    override fun applyUpdateEffect(entity: LivingEntity, amplifier: Int) {
        if (entity.health > 1.0f) {
            entity.damage(DamageSource.MAGIC, 1.0f)
        }
    }

    override fun canApplyUpdateEffect(duration: Int, amplifier: Int): Boolean {
        val i = 25 shr amplifier
        return if (i > 0) {
            duration % i == 0
        } else true
    }
}