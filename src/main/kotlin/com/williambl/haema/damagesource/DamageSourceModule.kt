package com.williambl.haema.damagesource

import com.williambl.haema.api.DamageSourceEfficacyEvent
import com.williambl.haema.util.vampiresDrown
import net.fabricmc.api.ModInitializer
import net.minecraft.entity.damage.DamageSource
import net.minecraft.world.World
import java.util.*

object DamageSourceModule: ModInitializer {
    private val damageSourcesThatCanKillVampires = WeakHashMap<DamageSource, Float>()

    fun DamageSource.setEffectiveAgainstVampires(multiplier: Float = 1.25f) {
        damageSourcesThatCanKillVampires[this] = multiplier
    }

    fun DamageSource.getModifier(world: World): Float {
        return DamageSourceEfficacyEvent.EVENT.invoker().getMultiplier(this, world)
    }

    fun DamageSource.isEffectiveAgainstVampires(world: World): Boolean {
        return this.getModifier(world) > 1f
    }

    override fun onInitialize() {
        DamageSourceEfficacyEvent.EVENT.register { source, world ->
            if (
                source.isOutOfWorld
                || source is SunlightDamageSource
                || source == DamageSource.LIGHTNING_BOLT
                || source == DamageSource.DRAGON_BREATH
                || source.isMagic
            ) {
                1.25f
            } else {
                1f
            }
        }

        DamageSourceEfficacyEvent.EVENT.register { source, world ->
            if (source == DamageSource.DROWN && world.gameRules.getBoolean(vampiresDrown)) {
                1.25f
            } else {
                1f
            }
        }

        DamageSourceEfficacyEvent.EVENT.register { source, world ->
            damageSourcesThatCanKillVampires.getOrDefault(source, 1f)
        }
    }
}