package com.williambl.haema.compat.rats

import com.williambl.haema.Vampirable
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.component.VampirePlayerComponent
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import ladysnake.ratsmischief.common.entity.RatEntity

fun initRatsMischiefIntegration() {
    BloodDrinkingEvents.ON_BLOOD_DRINK.register(BloodDrinkingEvents.DrinkBloodEvent { drinker, target, world ->
        if (target is RatEntity) {
            (target as Vampirable).isVampire = true
        }
    })
}

fun registerRatVampireComponent(registry: EntityComponentFactoryRegistry) {
    registry.registerFor(RatEntity::class.java, VampireComponent.entityKey) { entity -> VampirePlayerComponent(entity) }
}