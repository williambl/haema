package com.williambl.haema

import net.fabricmc.fabric.api.event.player.UseBlockCallback
import net.minecraft.item.Items
import net.minecraft.util.ActionResult

fun init() {
    UseBlockCallback.EVENT.register(UseBlockCallback { playerEntity, world, hand, blockHitResult ->
        if (playerEntity.getStackInHand(hand).item == Items.STICK) {
            (playerEntity.hungerManager as VampireBloodManager)
        }
        ActionResult.PASS
    })
}

