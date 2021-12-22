package com.williambl.haema.util

import net.minecraft.inventory.Inventory
import net.minecraft.item.Item

fun Inventory.contains(predicate: (Item) -> Boolean): Boolean {
    for (i in 0 until size()) {
        val itemStack = getStack(i)
        if (predicate(itemStack.item) && itemStack.count > 0) {
            return true
        }
    }
    return false
}