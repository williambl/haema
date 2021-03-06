package com.williambl.haema.ritual.craft

import net.minecraft.entity.ItemEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.fluid.Fluid
import net.minecraft.inventory.SimpleInventory
import net.minecraft.item.ItemStack
import net.minecraft.util.math.BlockPos

class RitualInventory(itemEntities: Collection<ItemEntity>, val fluid: Fluid, val pos: BlockPos, val player: PlayerEntity, val level: Int) : SimpleInventory(*itemEntities.map { it.stack }.toTypedArray()) {
    fun getAllItemStacks(): List<ItemStack> {
        val result = mutableListOf<ItemStack>()
        for (i in 0 until size()) {
            result.add(getStack(i))
        }
        return result.toList()
    }
}
