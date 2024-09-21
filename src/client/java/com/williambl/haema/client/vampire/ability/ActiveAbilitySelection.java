package com.williambl.haema.client.vampire.ability;

import com.williambl.haema.client.vampire.HaemaVampiresClient;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.fabricmc.fabric.api.client.rendering.v1.HudRenderCallback;
import net.minecraft.network.chat.Component;

public class ActiveAbilitySelection {

    public static void init() {
        ClientTickEvents.START_CLIENT_TICK.register(mc -> {
            if (mc.screen == null && HaemaVampiresClient.Keybinds.SELECT_VAMPIRE_ABILITY.consumeClick()) {
                mc.setScreen(new AbilitySelectionScreen(Component.empty()));
            }
        });
    }
}
