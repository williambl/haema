package com.williambl.haema.api

import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.minecraft.block.BlockState
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.util.Hand
import net.minecraft.util.hit.BlockHitResult
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

fun interface RitualTableUseEvent {
    companion object {
        val EVENT: Event<RitualTableUseEvent> = createArrayBacked(RitualTableUseEvent::class.java) { listeners ->
            RitualTableUseEvent { state, world, pos, player, hand, hit ->
                for (listener in listeners) {
                    listener.onUse(state, world, pos, player, hand, hit)
                }
            }
        }
    }

    fun onUse(state: BlockState,
              world: World,
              pos: BlockPos,
              player: PlayerEntity,
              hand: Hand,
              hit: BlockHitResult
    )
}
