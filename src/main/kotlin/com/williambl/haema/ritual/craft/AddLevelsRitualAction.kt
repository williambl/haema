package com.williambl.haema.ritual.craft

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.ritual.RitualTableScreenHandler
import com.williambl.haema.setAbilityLevel
import net.minecraft.nbt.AbstractNbtNumber
import net.minecraft.nbt.NbtElement
import net.minecraft.text.MutableText
import net.minecraft.text.Text

object AddLevelsRitualAction: RitualAction() {
    override fun runAction(inv: RitualInventory, data: NbtElement) {
        val levels = if (data is AbstractNbtNumber) {
            data.intValue()
        } else 0

        inv.player.setAbilityLevel(
            AbilityModule.NONE,
            inv.player.getAbilityLevel(AbilityModule.NONE)+levels
        )
        inv.player.openHandledScreen(RitualTableScreenHandler.Factory(inv))
    }

    override fun getName(data: NbtElement): MutableText {
        return Text.translatable(
            this.translationKey,
            if (data is AbstractNbtNumber) {
                data.intValue()
            } else 0
        )
    }
}