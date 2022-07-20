package com.williambl.haema.util

import net.minecraft.client.gui.screen.Screen
import net.minecraft.text.Text

/**
 * A screen which closes itself immediately. This is used in [com.williambl.haema.compat.rei.HaemaREIPlugin], but cannot
 * go in that package because REI ignores screens in a `.rei.` package.
 *
 * @constructor Create a Self-closing screen
 */
class SelfClosingScreen : Screen(Text.of("closing...")) {
    override fun tick() {
        this.close()
    }
}