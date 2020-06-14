package com.williambl.haema.damagesource

import net.minecraft.entity.damage.DamageSource

class SunlightDamageSource : DamageSource("sunlight") {
    companion object {
        val instance: DamageSource = SunlightDamageSource().setBypassesArmor()
    }
}