package com.williambl.haema.ritual.craft

import com.williambl.haema.ritual.RitualModule
import net.minecraft.nbt.NbtElement
import net.minecraft.text.MutableText
import net.minecraft.text.Text
import net.minecraft.util.Util

abstract class RitualAction {
    protected val translationKey: String by lazy { Util.createTranslationKey("ritual_action", RitualModule.RITUAL_ACTION_REGISTRY.getId(this)) }

    abstract fun runAction(inv: RitualInventory, data: NbtElement)
    abstract fun getName(data: NbtElement): MutableText
}