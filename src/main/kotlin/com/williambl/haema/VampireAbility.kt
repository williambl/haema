package com.williambl.haema

enum class VampireAbility(val maxLevel: Int = Int.MAX_VALUE) {
    NONE,
    STRENGTH(3),
    DASH(1),
    INVISIBILITY(1),
    IMMORTALITY(1),
    VISION(1)
}