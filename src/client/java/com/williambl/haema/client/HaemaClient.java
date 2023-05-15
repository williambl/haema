package com.williambl.haema.client;

import com.williambl.haema.client.vampire.HaemaVampiresClient;
import net.fabricmc.api.ClientModInitializer;

public class HaemaClient implements ClientModInitializer {
    @Override
    public void onInitializeClient() {
        HaemaVampiresClient.init();
    }
}
