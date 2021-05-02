package com.williambl.haema.api

import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.entity.damage.DamageSource
import net.minecraft.world.World

fun interface DamageSourceEfficacyEvent {
    companion object {
        val EVENT: Event<DamageSourceEfficacyEvent> = createArrayBacked(DamageSourceEfficacyEvent::class.java) { listeners ->
            DamageSourceEfficacyEvent { player, ability ->
                for (listener in listeners) {
                    when (listener.isDamageSourceEffective(player, ability)) {
                        TriState.TRUE -> return@DamageSourceEfficacyEvent TriState.TRUE
                        TriState.FALSE -> return@DamageSourceEfficacyEvent TriState.FALSE
                        TriState.DEFAULT -> continue
                    }
                }
                TriState.DEFAULT
            }
        }
    }

    fun isDamageSourceEffective(source: DamageSource, world: World): TriState
}
