package com.williambl.haema.abilities

import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions

open class VampireAbility(val maxLevel: Int = Int.MAX_VALUE, val iconItem: ItemStack = ItemStack.EMPTY) {
    open fun isVisible(player: PlayerEntity): Boolean = true

    companion object {
        val NONE = VampireAbility()
        val STRENGTH = VampireAbility(3, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.STRENGTH))
        val DASH = VampireAbility(3, ItemStack(Items.FEATHER))
        val INVISIBILITY = VampireAbility(2, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.INVISIBILITY))
        val IMMORTALITY = VampireAbility(1, ItemStack(Items.TOTEM_OF_UNDYING))
        val VISION = VampireAbility(1, ItemStack(Items.ENDER_EYE))
    }
}