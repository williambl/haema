package com.williambl.haema.ritual.craft

import com.williambl.haema.ritual.RitualTableScreenHandler
import net.minecraft.nbt.NbtElement

object ChangeAbilitiesRitualAction: RitualAction {
    override fun runAction(inv: RitualInventory, data: NbtElement) {
        inv.player.openHandledScreen(RitualTableScreenHandler.Factory(inv))
    }
}