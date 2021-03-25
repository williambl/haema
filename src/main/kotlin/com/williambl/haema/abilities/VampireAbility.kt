package com.williambl.haema.abilities

import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions

open class VampireAbility(val maxLevel: Int = Int.MAX_VALUE, val iconItem: ItemStack = ItemStack.EMPTY) {
    companion object {
        public val NONE = VampireAbility()
        public val STRENGTH = VampireAbility(3, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.STRENGTH))
        public val DASH = VampireAbility(3, ItemStack(Items.FEATHER))
        public val INVISIBILITY = VampireAbility(2, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.INVISIBILITY))
        public val IMMORTALITY = VampireAbility(1, ItemStack(Items.TOTEM_OF_UNDYING))
        public val VISION = VampireAbility(1, ItemStack(Items.ENDER_EYE))
    }
}