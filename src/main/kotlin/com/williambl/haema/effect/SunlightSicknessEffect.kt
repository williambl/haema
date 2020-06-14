package com.williambl.haema.effect

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.damage.DamageSource
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.entity.effect.StatusEffectType
import net.minecraft.entity.player.PlayerEntity
import kotlin.math.max

class SunlightSicknessEffect(type: StatusEffectType?, color: Int) : StatusEffect(type, color) {

    companion object {
        val instance: StatusEffect = SunlightSicknessEffect(StatusEffectType.HARMFUL, 245 shl 24 or 167 shl 16 or 66 shl 8)
                .addAttributeModifier(
                        EntityAttributes.GENERIC_ATTACK_DAMAGE,
                        "c85d1cfe-2c10-4d25-b650-49c045979842",
                        -4.0,
                        EntityAttributeModifier.Operation.ADDITION
                )
    }

    override fun applyUpdateEffect(entity: LivingEntity?, amplifier: Int) {
        if (entity !is PlayerEntity)
            return

        if (entity.age % 10 == 0) {
            entity.damage(DamageSource.MAGIC, 0.25f)
            (entity.hungerManager as VampireBloodManager).removeBlood(0.01)
        }
    }

    override fun canApplyUpdateEffect(duration: Int, amplifier: Int): Boolean {
        return true
    }
}