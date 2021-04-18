package com.williambl.haema.api.client

import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.text.MutableText
import net.minecraft.text.Text

fun interface VampireHudAddTextEvent {
    companion object {
        val EVENT: Event<VampireHudAddTextEvent> =
            EventFactory.createArrayBacked(VampireHudAddTextEvent::class.java) { listeners ->
                VampireHudAddTextEvent { player, createText ->
                    listeners.flatMap { it.addText(player, createText) }
                }
            }
    }

    fun addText(player: PlayerEntity,
                createText: (MutableText, Boolean, Text) -> Text): Collection<Text>
}