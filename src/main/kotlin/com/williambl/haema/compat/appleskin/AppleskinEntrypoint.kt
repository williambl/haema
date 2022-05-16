package com.williambl.haema.compat.appleskin

import com.williambl.haema.isVampire
import net.minecraft.client.MinecraftClient
import squeek.appleskin.api.AppleSkinApi
import squeek.appleskin.api.event.HUDOverlayEvent
import squeek.appleskin.api.event.TooltipOverlayEvent

object AppleskinEntrypoint: AppleSkinApi {
    override fun registerEvents() {
        HUDOverlayEvent.Exhaustion.EVENT.register { event ->
            if (MinecraftClient.getInstance().player?.isVampire == true) {
                event.isCanceled = true
            }
        }

        HUDOverlayEvent.HungerRestored.EVENT.register { event ->
            if (MinecraftClient.getInstance().player?.isVampire == true) {
                event.isCanceled = true
            }
        }

        HUDOverlayEvent.HealthRestored.EVENT.register { event ->
            if (MinecraftClient.getInstance().player?.isVampire == true) {
                event.isCanceled = true
            }
        }

        HUDOverlayEvent.Saturation.EVENT.register { event ->
            if (MinecraftClient.getInstance().player?.isVampire == true) {
                event.isCanceled = true
            }
        }

        TooltipOverlayEvent.Pre.EVENT.register { event ->
            if (MinecraftClient.getInstance().player?.isVampire == true) {
                event.isCanceled = true
            }
        }
    }
}