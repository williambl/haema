package com.williambl.haema.api

import com.williambl.haema.api.BloodDrinkingEvents.CancelBloodDrinkEvent
import com.williambl.haema.api.BloodDrinkingEvents.DrinkBloodEvent
import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.minecraft.entity.LivingEntity
import net.minecraft.util.Hand
import net.minecraft.util.hit.EntityHitResult
import net.minecraft.world.World

object BloodDrinkingEvents {
    val ON_BLOOD_DRINK: Event<DrinkBloodEvent> = createArrayBacked(DrinkBloodEvent::class.java) { listeners ->
        DrinkBloodEvent { drinker, world, target ->
            for (listener in listeners) {
                listener.onDrink(drinker, world, target)
            }
        }
    }

    val CANCEL: Event<CancelBloodDrinkEvent> = createArrayBacked(CancelBloodDrinkEvent::class.java) { listeners ->
        CancelBloodDrinkEvent { player, world, hand, target, entityHitResult ->
            for (listener in listeners) {
                if (!listener.canDrink(player, world, hand, target, entityHitResult)) return@CancelBloodDrinkEvent false
            }
            return@CancelBloodDrinkEvent true
        }
    }

    fun interface DrinkBloodEvent {
        fun onDrink(drinker: LivingEntity, target: LivingEntity, world: World)
    }

    fun interface CancelBloodDrinkEvent {
        fun canDrink(player: LivingEntity, world: World, hand: Hand, target: LivingEntity, entityHitResult: EntityHitResult?): Boolean
    }
}