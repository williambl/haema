package com.williambl.haema.api

import com.williambl.haema.abilities.VampireAbility
import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.world.World

fun interface AbilityVisibilityEvent {
    companion object {
        val EVENT: Event<AbilityVisibilityEvent> = createArrayBacked(AbilityVisibilityEvent::class.java) { listeners ->
            AbilityVisibilityEvent { player, ability ->
                for (listener in listeners) {
                    when (listener.onVisibilityTest(player, ability)) {
                        TriState.TRUE -> return@AbilityVisibilityEvent TriState.TRUE
                        TriState.FALSE -> return@AbilityVisibilityEvent TriState.FALSE
                        TriState.DEFAULT -> continue
                    }
                }
                TriState.DEFAULT
            }
        }
    }

    fun onVisibilityTest(player: PlayerEntity, ability: VampireAbility): TriState
}
