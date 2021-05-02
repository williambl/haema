package com.williambl.haema.damagesource

import com.williambl.haema.api.DamageSourceEfficacyEvent
import com.williambl.haema.util.vampiresDrown
import net.minecraft.entity.damage.DamageSource
import net.minecraft.world.World
import java.lang.ref.WeakReference

val damageSourcesThatCanKillVampires = mutableSetOf<WeakReference<DamageSource>>()

fun DamageSource.setEffectiveAgainstVampires() {
    damageSourcesThatCanKillVampires.add(WeakReference(this))
}

fun DamageSource.isEffectiveAgainstVampires(world: World): Boolean {
    return DamageSourceEfficacyEvent.EVENT.invoker().isDamageSourceEffective(this, world).orElseGet {
        this.isOutOfWorld
                || this is SunlightDamageSource
                || damageSourcesThatCanKillVampires.any { it.get() == this }
                || this == DamageSource.LIGHTNING_BOLT
                || (this == DamageSource.DROWN && world.gameRules.getBoolean(vampiresDrown))
                || this == DamageSource.DRAGON_BREATH
                || this.isOutOfWorld
                || this.magic
    }
}
