package com.williambl.haema.effect

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.entity.effect.StatusEffectType

class VampiricStrengthEffect(type: StatusEffectType?, color: Int) : StatusEffect(type, color) {

    companion object {
        val instance: StatusEffect = VampiricStrengthEffect(StatusEffectType.BENEFICIAL, 171 shl 24 or 12 shl 16 or 12 shl 8)
            .addAttributeModifier(
                EntityAttributes.GENERIC_ATTACK_DAMAGE,
                "beb69dfa-5de3-4f82-82a5-29f5ba715a18",
                4.0,
                EntityAttributeModifier.Operation.ADDITION
            )
            .addAttributeModifier(
                EntityAttributes.GENERIC_ATTACK_SPEED,
                "3ca8311c-601a-44f9-97ee-2b0677247e64",
                0.3,
                EntityAttributeModifier.Operation.MULTIPLY_TOTAL
            )
            .addAttributeModifier(
                EntityAttributes.GENERIC_MOVEMENT_SPEED,
                "7a47b1b8-16a5-4877-905a-07ffd5d2189b",
                0.2,
                EntityAttributeModifier.Operation.MULTIPLY_TOTAL
            )
    }

    override fun applyUpdateEffect(entity: LivingEntity?, amplifier: Int) {}

    override fun canApplyUpdateEffect(duration: Int, amplifier: Int): Boolean { return false }

}