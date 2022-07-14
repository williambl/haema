package com.williambl.haema.util

import net.minecraft.inventory.Inventory
import net.minecraft.item.ItemStack

fun Inventory.contains(predicate: (ItemStack) -> Boolean): Boolean {
    for (i in 0 until size()) {
        val itemStack = getStack(i)
        if (predicate(itemStack) && itemStack.count > 0) {
            return true
        }
    }
    return false
}