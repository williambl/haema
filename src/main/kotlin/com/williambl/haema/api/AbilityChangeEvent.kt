package com.williambl.haema.api

import com.williambl.haema.Vampirable
import com.williambl.haema.abilities.VampireAbility
import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked

fun interface AbilityChangeEvent {
    companion object {
        val EVENT: Event<AbilityChangeEvent> = createArrayBacked(AbilityChangeEvent::class.java) { listeners ->
            AbilityChangeEvent { vampire, ability, level ->
                for (listener in listeners) {
                    listener.onAbilityChange(vampire, ability, level)
                }
            }
        }
    }

    fun onAbilityChange(vampire: Vampirable, ability: VampireAbility, level: Int)
}
