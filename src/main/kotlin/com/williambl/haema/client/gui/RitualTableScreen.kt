package com.williambl.haema.client.gui

import com.williambl.haema.VampireAbility
import com.williambl.haema.ritual.RitualTableScreenHandler
import net.minecraft.client.gui.screen.ingame.HandledScreen
import net.minecraft.client.util.math.MatrixStack
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.text.Text

class RitualTableScreen(handler: RitualTableScreenHandler, inventory: PlayerInventory, title: Text) :
    HandledScreen<RitualTableScreenHandler>(handler, inventory, title) {
    override fun drawBackground(matrices: MatrixStack?, delta: Float, mouseX: Int, mouseY: Int) {
        VampireAbility.values().forEachIndexed { idx, ability ->
            println("${ability.name}: ${handler.getProperty(idx)}")
        }
    }
}