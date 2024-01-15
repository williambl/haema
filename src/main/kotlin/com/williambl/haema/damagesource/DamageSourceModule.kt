package com.williambl.haema.damagesource

import com.williambl.haema.api.DamageSourceEfficacyEvent
import com.williambl.haema.damagetype.DamageTypeModule
import com.williambl.haema.util.HaemaGameRules
import net.fabricmc.api.ModInitializer
import net.minecraft.entity.damage.DamageSource
import net.minecraft.entity.damage.DamageSources
import net.minecraft.entity.damage.DamageTypes
import net.minecraft.world.World
import java.util.*

fun DamageSources.bloodLoss(): DamageSource {
    return DamageSourceModule.getCachedDamageSources(this).bloodLoss
}

fun DamageSources.incompatibleBlood(): DamageSource {
    return DamageSourceModule.getCachedDamageSources(this).incompatibleBlood
}

fun DamageSources.sunlight(): DamageSource {
    return DamageSourceModule.getCachedDamageSources(this).sunlight
}

object DamageSourceModule : ModInitializer {
    private val damageSourcesCache = WeakHashMap<DamageSources, CachedDamageSources>()
    private val damageSourcesThatCanKillVampires = WeakHashMap<DamageSource, Float>()

    fun getCachedDamageSources(damageSources: DamageSources): CachedDamageSources {
        return damageSourcesCache.getOrPut(damageSources) {
            CachedDamageSources(
                bloodLoss = damageSources.create(DamageTypeModule.BLOOD_LOSS),
                incompatibleBlood = damageSources.create(DamageTypeModule.INCOMPATIBLE_BLOOD),
                sunlight = damageSources.create(DamageTypeModule.SUNLIGHT),
            )
        }
    }

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
                source.isOf(DamageTypes.OUT_OF_WORLD)
                || source.isOf(DamageTypes.OUTSIDE_BORDER)
                || source.isOf(DamageTypeModule.SUNLIGHT)
                || source.isOf(DamageTypes.LIGHTNING_BOLT)
                || source.isOf(DamageTypes.DRAGON_BREATH)
                || source.isOf(DamageTypes.MAGIC)
                || source.isOf(DamageTypes.INDIRECT_MAGIC)
            ) {
                1.25f
            } else {
                1f
            }
        }

        DamageSourceEfficacyEvent.EVENT.register { source, world ->
            if (source.isOf(DamageTypes.DROWN) && world.gameRules.getBoolean(HaemaGameRules.vampiresDrown)) {
                1.25f
            } else {
                1f
            }
        }

        DamageSourceEfficacyEvent.EVENT.register { source, world ->
            damageSourcesThatCanKillVampires.getOrDefault(source, 1f)
        }
    }

    data class CachedDamageSources(
        val bloodLoss: DamageSource,
        val incompatibleBlood: DamageSource,
        val sunlight: DamageSource,
    )
}