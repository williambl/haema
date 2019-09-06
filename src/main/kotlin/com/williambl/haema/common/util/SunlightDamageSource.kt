package com.williambl.haema.common.util

import net.minecraft.util.DamageSource

object SunlightDamageSource : DamageSource("sunlight") {
    init {
        this.setDamageBypassesArmor()
        this.setMagicDamage()
    }
}