package com.williambl.haema.compat.flan

import com.williambl.haema.api.BloodDrinkingEvents
import io.github.flemmli97.flan.api.ClaimPermission
import io.github.flemmli97.flan.api.PermissionRegistry
import io.github.flemmli97.flan.claim.ClaimStorage
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry

fun initFlanIntegration() {
    val permission = PermissionRegistry.global(ClaimPermission(
        "DRINKBLOOD",
        Registry.ITEM.get(Identifier("haema:vampire_blood_injector"))::getDefaultStack,
        "Allow players to drink blood"
    ))

    BloodDrinkingEvents.CANCEL.register(BloodDrinkingEvents.CancelBloodDrinkEvent { player, world, hand, target, entityHitResult ->
        if (entityHitResult != null && world is ServerWorld) {
            permission.test.test(ClaimStorage.get(world).getClaimAt(player.blockPos), player, player.blockPos) != ClaimPermission.PermissionFlag.NO
        } else {
            true
        }
    })
}
