package com.williambl.haema.compat.bewitchment

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.api.BloodDrinkingEvents
import moriyashiine.bewitchment.api.BewitchmentAPI
import moriyashiine.bewitchment.api.event.BloodSetEvents
import moriyashiine.bewitchment.api.event.BloodSuckEvents
import moriyashiine.bewitchment.api.event.OnTransformationSet
import moriyashiine.bewitchment.api.interfaces.entity.TransformationAccessor
import moriyashiine.bewitchment.common.registry.BWTransformations
import net.minecraft.entity.player.PlayerEntity

fun registerBewitchmentEventListeners() {
    BloodSetEvents.ON_BLOOD_FILL.register(BloodSetEvents.OnFillBlood { entity, amount, simulated ->
        if (!simulated) {
            if (entity is Vampirable && entity is PlayerEntity && entity.hungerManager is VampireBloodManager) {
                (entity.hungerManager as VampireBloodManager).addBlood(amount*0.2)
            }
        }
    })
    BloodSetEvents.ON_BLOOD_DRAIN.register(BloodSetEvents.OnDrainBlood { entity, amount, simulated ->
        if (!simulated) {
            if (entity is Vampirable && entity is PlayerEntity && entity.hungerManager is VampireBloodManager) {
                (entity.hungerManager as VampireBloodManager).removeBlood(amount*0.2)
            }
        }
    })
    BloodSuckEvents.BLOOD_AMOUNT.register(BloodSuckEvents.SetBloodAmount { player, target, currentBloodToGive ->
        when {
            VampireBloodManager.goodBloodTag.contains(target.type) -> 5
            VampireBloodManager.mediumBloodTag.contains(target.type) -> 2
            VampireBloodManager.poorBloodTag.contains(target.type) -> 1
            else -> currentBloodToGive
        }
    })
    OnTransformationSet.EVENT.register(OnTransformationSet { player, transformation ->
        if (transformation == BWTransformations.VAMPIRE) {
            (player as Vampirable).isVampire = true
            player.isPermanentVampire = true
        } else if (transformation != BWTransformations.VAMPIRE && (player as TransformationAccessor).transformation == BWTransformations.VAMPIRE) {
            (player as Vampirable).isVampire = false
            player.isPermanentVampire = false
        }
    })
    BloodDrinkingEvents.CANCEL.register(BloodDrinkingEvents.CancelBloodDrinkEvent { player, world, hand, target, entityHitResult ->
        !BewitchmentAPI.isVampire(player, true)
    })
}