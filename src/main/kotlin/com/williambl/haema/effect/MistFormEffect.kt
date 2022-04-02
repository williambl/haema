package com.williambl.haema.effect

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.entity.effect.StatusEffectCategory

class MistFormEffect(type: StatusEffectCategory?, color: Int) : StatusEffect(type, color) {

    companion object {
        val instance: StatusEffect = MistFormEffect(StatusEffectCategory.NEUTRAL, 0x89bfa9)
            .addAttributeModifier(
                EntityAttributes.GENERIC_ATTACK_DAMAGE,
                "4439d268-8182-438b-88c9-baf60305c828",
                Double.NEGATIVE_INFINITY,
                EntityAttributeModifier.Operation.ADDITION
            )
            .addAttributeModifier(
                EntityAttributes.GENERIC_ARMOR,
                "b946d133-ce1a-40e8-8814-005350fd34bc",
                Double.NEGATIVE_INFINITY,
                EntityAttributeModifier.Operation.MULTIPLY_TOTAL
            )
    }

    override fun applyUpdateEffect(entity: LivingEntity?, amplifier: Int) {}

    override fun canApplyUpdateEffect(duration: Int, amplifier: Int): Boolean { return false }

}