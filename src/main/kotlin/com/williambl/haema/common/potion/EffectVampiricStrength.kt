package com.williambl.haema.common.potion

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.SharedMonsterAttributes
import net.minecraft.entity.ai.attributes.AttributeModifier
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.potion.AttackDamageEffect
import net.minecraft.potion.EffectType
import kotlin.math.max

class EffectVampiricStrength : AttackDamageEffect(EffectType.BENEFICIAL, 3484199, 4.0) {

    init {
        setRegistryName("vampiric_strength")
        addAttributesModifier(SharedMonsterAttributes.ATTACK_DAMAGE, "dea0d7eb-acd2-46b3-9f04-41ca23d5d7fc", 0.0, AttributeModifier.Operation.ADDITION)
    }

    override fun getCurativeItems(): MutableList<ItemStack> {
        return mutableListOf()
    }

    override fun isReady(duration: Int, amplifier: Int): Boolean {
        return true
    }

    override fun performEffect(entityLivingBaseIn: LivingEntity, amplifier: Int) {
        (entityLivingBaseIn as PlayerEntity).foodStats.addStats(amplifier + 1, 1.0F)
        entityLivingBaseIn.heal(max(4 shl amplifier, 0).toFloat())
    }
}