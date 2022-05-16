package com.williambl.haema.ritual.craft

import net.minecraft.nbt.NbtElement

interface RitualAction {
    fun runAction(inv: RitualInventory, data: NbtElement)
}