package com.williambl.haema.api

import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.minecraft.entity.damage.DamageSource
import net.minecraft.world.World

fun interface DamageSourceEfficacyEvent {
    companion object {
        val EVENT: Event<DamageSourceEfficacyEvent> =
            createArrayBacked(DamageSourceEfficacyEvent::class.java) { listeners ->
                DamageSourceEfficacyEvent { source, world ->
                    var multiplier = 1f
                    for (listener in listeners) {
                        multiplier *= listener.getMultiplier(source, world)
                    }
                    multiplier
                }
            }
    }

    fun getMultiplier(source: DamageSource, world: World): Float
}
