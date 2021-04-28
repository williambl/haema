package com.williambl.haema.api

import com.williambl.haema.api.BloodChangeEvents.AddBloodEvent
import com.williambl.haema.api.BloodChangeEvents.RemoveBloodEvent
import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.minecraft.entity.player.PlayerEntity

object BloodChangeEvents {
    val ON_BLOOD_ADD: Event<AddBloodEvent> = createArrayBacked(AddBloodEvent::class.java) { listeners ->
        AddBloodEvent { drinker, amount ->
            for (listener in listeners) {
                listener.onAdd(drinker, amount)
            }
        }
    }

    val ON_BLOOD_REMOVE: Event<RemoveBloodEvent> = createArrayBacked(RemoveBloodEvent::class.java) { listeners ->
        RemoveBloodEvent { player, amount ->
            for (listener in listeners) {
                listener.onRemove(player, amount)
            }
        }
    }

    fun interface AddBloodEvent {
        fun onAdd(player: PlayerEntity, amount: Double)
    }

    fun interface RemoveBloodEvent {
        fun onRemove(player: PlayerEntity, amount: Double)
    }
}