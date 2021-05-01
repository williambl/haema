package com.williambl.haema.compat.flan

import com.flemmli97.flan.event.EntityInteractEvents
import com.williambl.haema.api.BloodDrinkingEvents

fun initFlanIntegration() {
        BloodDrinkingEvents.CANCEL.register(BloodDrinkingEvents.CancelBloodDrinkEvent { player, world, hand, target, entityHitResult ->
            if (entityHitResult != null) {
                !EntityInteractEvents.attackSimple(player, entityHitResult.entity, true).isAccepted
            } else {
                true
            }
        })
}
