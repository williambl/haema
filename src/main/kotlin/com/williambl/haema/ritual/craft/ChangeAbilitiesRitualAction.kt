package com.williambl.haema.ritual.craft

import com.williambl.haema.ritual.RitualTableScreenHandler
import net.minecraft.nbt.NbtElement
import net.minecraft.text.MutableText
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText

object ChangeAbilitiesRitualAction: RitualAction() {
    override fun runAction(inv: RitualInventory, data: NbtElement) {
        inv.player.openHandledScreen(RitualTableScreenHandler.Factory(inv))
    }

    override fun getName(data: NbtElement): MutableText {
        return TranslatableText(this.translationKey)
    }
}