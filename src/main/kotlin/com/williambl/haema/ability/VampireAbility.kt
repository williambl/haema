package com.williambl.haema.ability

import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack

open class VampireAbility(val maxLevel: Int = Int.MAX_VALUE, val iconItem: ItemStack = ItemStack.EMPTY) {
    open fun isVisible(player: PlayerEntity): Boolean = true
}