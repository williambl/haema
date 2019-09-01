package com.williambl.haema.common.util

enum class VampireAbilities {
    WEAKNESS,
    STRENGTH,
    VISION,
    CHARISMA,
    FLIGHT,
    INVISIBILITY;

    val flag = 1 shl this.ordinal
}