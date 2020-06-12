package com.williambl.haema

import net.fabricmc.fabric.api.event.player.UseBlockCallback
import net.fabricmc.fabric.api.network.PacketRegistry
import net.minecraft.item.Items
import net.minecraft.util.ActionResult
import net.minecraft.util.Identifier

val bloodLevelPackeId = Identifier("haema:bloodlevelsync")

fun init() {
    UseBlockCallback.EVENT.register(UseBlockCallback { playerEntity, world, hand, blockHitResult ->
        if (playerEntity.getStackInHand(hand).item == Items.STICK) {
            (playerEntity.hungerManager as VampireBloodManager).addBlood(0.1)
        } else if (playerEntity.getStackInHand(hand).item == Items.GOLD_NUGGET) {
            println((playerEntity.hungerManager as VampireBloodManager).absoluteBloodLevel)
        }
        ActionResult.PASS
    })
}

