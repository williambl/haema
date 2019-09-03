package com.williambl.haema.common.potion

import net.minecraft.entity.EntityLivingBase
import net.minecraft.entity.SharedMonsterAttributes
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.potion.PotionAttackDamage
import net.minecraft.util.DamageSource

class PotionVampiricWeakness : PotionAttackDamage(true, 3484199, -4.0) {

    init {
        setRegistryName("vampiric_weakness")
        setPotionName("effect.vampiric_weakness")
        registerPotionAttributeModifier(SharedMonsterAttributes.ATTACK_DAMAGE, "99fe3f3c-eee1-4553-b7ab-f262590ea1a7", 0.0, 0)
    }

    override fun getCurativeItems(): MutableList<ItemStack> {
        return mutableListOf()
    }

    override fun isReady(duration: Int, amplifier: Int): Boolean {
        return true
    }

    override fun performEffect(entityLivingBaseIn: EntityLivingBase, amplifier: Int) {
        (entityLivingBaseIn as EntityPlayer).addExhaustion(0.01F * (amplifier.toFloat() + 1))
        if (amplifier > 1)
            entityLivingBaseIn.attackEntityFrom(DamageSource.MAGIC, (1 shl amplifier) * 0.1f)
    }
}