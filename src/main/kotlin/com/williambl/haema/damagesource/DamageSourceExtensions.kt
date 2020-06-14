package com.williambl.haema.damagesource

import net.minecraft.entity.damage.DamageSource
import net.minecraft.entity.damage.EntityDamageSource
import java.lang.ref.WeakReference

val damageSourcesThatCanKillVampires = mutableSetOf<WeakReference<DamageSource>>()

fun DamageSource.setEffectiveAgainstVampires() {
    damageSourcesThatCanKillVampires.add(WeakReference(this))
}

fun DamageSource.isEffectiveAgainstVampires(): Boolean {
    return this.isOutOfWorld || this is SunlightDamageSource || damageSourcesThatCanKillVampires.any { it.get() == this }
}
