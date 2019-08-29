package com.williambl.haema.common.potion

import net.minecraft.entity.EntityLivingBase
import net.minecraft.entity.SharedMonsterAttributes
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.potion.PotionAttackDamage
import kotlin.math.max

class PotionVampiricStrength : PotionAttackDamage(false, 3484199, 4.0) {

    init {
        setRegistryName("vampiric_strength")
        setPotionName("effect.vampiric_strength")
        registerPotionAttributeModifier(SharedMonsterAttributes.ATTACK_DAMAGE, "dea0d7eb-acd2-46b3-9f04-41ca23d5d7fc", 0.0, 0)
    }

    override fun getCurativeItems(): MutableList<ItemStack> {
        return mutableListOf()
    }

    override fun isReady(duration: Int, amplifier: Int): Boolean {
        return true
    }

    override fun performEffect(entityLivingBaseIn: EntityLivingBase, amplifier: Int) {
        (entityLivingBaseIn as EntityPlayer).foodStats.addStats(amplifier + 1, 1.0F)
        entityLivingBaseIn.heal(max(4 shl amplifier, 0).toFloat())
    }
}