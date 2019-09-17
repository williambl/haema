package com.williambl.haema.common.potion

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.SharedMonsterAttributes
import net.minecraft.entity.ai.attributes.AttributeModifier
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.potion.AttackDamageEffect
import net.minecraft.potion.EffectType

class EffectVampiricWeakness : AttackDamageEffect(EffectType.HARMFUL, 3484199, -4.0) {

    init {
        setRegistryName("vampiric_weakness")
        addAttributesModifier(SharedMonsterAttributes.ATTACK_DAMAGE, "99fe3f3c-eee1-4553-b7ab-f262590ea1a7", 0.0, AttributeModifier.Operation.ADDITION)
    }

    override fun getCurativeItems(): MutableList<ItemStack> {
        return mutableListOf()
    }

    override fun isReady(duration: Int, amplifier: Int): Boolean {
        return true
    }

    override fun performEffect(entityLivingBaseIn: LivingEntity, amplifier: Int) {
        (entityLivingBaseIn as PlayerEntity).addExhaustion(0.01F * (amplifier.toFloat() + 1))
    }
}