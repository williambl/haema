package com.williambl.haema

import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions

enum class VampireAbility(val maxLevel: Int = Int.MAX_VALUE, val iconItem: ItemStack = ItemStack.EMPTY) {
    NONE,
    STRENGTH(3, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.STRENGTH)),
    DASH(1, ItemStack(Items.FEATHER)),
    INVISIBILITY(1, PotionUtil.setPotion(ItemStack(Items.POTION), Potions.INVISIBILITY)),
    IMMORTALITY(1, ItemStack(Items.TOTEM_OF_UNDYING)),
    VISION(1, ItemStack(Items.ENDER_EYE))
}