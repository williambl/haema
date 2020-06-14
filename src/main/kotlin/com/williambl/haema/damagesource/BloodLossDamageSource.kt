package com.williambl.haema.damagesource

import net.minecraft.entity.damage.DamageSource

class BloodLossDamageSource : DamageSource("bloodLoss") {
    companion object {
        val instance: DamageSource = BloodLossDamageSource().setBypassesArmor()
    }
}