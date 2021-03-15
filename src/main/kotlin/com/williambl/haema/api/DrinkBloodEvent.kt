package com.williambl.haema.api

import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.minecraft.entity.Entity
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.world.World

fun interface DrinkBloodEvent {
    companion object {
        val EVENT: Event<DrinkBloodEvent> = createArrayBacked(DrinkBloodEvent::class.java) { listeners ->
            DrinkBloodEvent { drinker, world, target ->
                for (listener in listeners) {
                    listener.onDrink(drinker, world, target)
                }
            }
        }
    }

    fun onDrink(drinker: PlayerEntity, target: LivingEntity, world: World)
}
