package com.williambl.haema.damagesource

import net.minecraft.entity.damage.DamageSource

class IncompatibleBloodDamageSource : DamageSource("incompatibleBlood") {
    companion object {
        val instance: DamageSource = IncompatibleBloodDamageSource().setBypassesArmor()
    }
}