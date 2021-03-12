package com.williambl.haema.api

import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.world.World

fun interface WillVampireBurn {
    companion object {
        val EVENT: Event<WillVampireBurn> = createArrayBacked(WillVampireBurn::class.java) { listeners -> WillVampireBurn { player, world ->
            val result = listeners.map { it.willVampireBurn(player, world) }
            when {
                result.any { it == TriState.FALSE } -> {
                    TriState.FALSE
                }
                result.any { it == TriState.TRUE } -> {
                    TriState.TRUE
                }
                else -> TriState.DEFAULT
            }
        }}
    }

    /**
     * Just one callback returning [TriState.FALSE] will cancel the burning.
     * If nothing returns [TriState.TRUE], no burning occurs.
     */
    fun willVampireBurn(player: PlayerEntity, world: World): TriState
}