package com.williambl.haema.common.util

enum class VampireAbilities {
    WEAKNESS,
    STRENGTH,
    VISION,
    FLIGHT,
    INVISIBILITY;

    val flag = 1 shl this.ordinal
}