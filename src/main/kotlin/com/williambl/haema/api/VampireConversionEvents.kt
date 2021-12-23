package com.williambl.haema.api

import com.williambl.haema.api.VampireConversionEvents.ConversionEvent
import com.williambl.haema.api.VampireConversionEvents.DeconversionEvent
import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.minecraft.entity.player.PlayerEntity

object VampireConversionEvents {
    val CONVERT: Event<ConversionEvent> = createArrayBacked(ConversionEvent::class.java) { listeners -> ConversionEvent { player ->
        for (listener in listeners) {
            listener.onConvert(player)
        }
    }}

    val DECONVERT: Event<DeconversionEvent> = createArrayBacked(DeconversionEvent::class.java) { listeners -> DeconversionEvent { player ->
        for (listener in listeners) {
            listener.onDeconvert(player)
        }
    }}

    fun interface ConversionEvent {
        fun onConvert(player: PlayerEntity)
    }

    fun interface DeconversionEvent {
        fun onDeconvert(player: PlayerEntity)
    }
}
