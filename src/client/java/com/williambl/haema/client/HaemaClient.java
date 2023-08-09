package com.williambl.haema.client;

import com.williambl.haema.client.client.content.HaemaContentClient;
import com.williambl.haema.client.hunters.HaemaHuntersClient;
import com.williambl.haema.client.ritual.HaemaRitualsClient;
import com.williambl.haema.client.vampire.HaemaVampiresClient;
import com.williambl.haema.client.vampire_mobs.HaemaVampireMobsClient;
import net.fabricmc.api.ClientModInitializer;

public class HaemaClient implements ClientModInitializer {
    @Override
    public void onInitializeClient() {
        HaemaVampiresClient.init();
        HaemaContentClient.init();
        HaemaRitualsClient.init();
        HaemaHuntersClient.init();
        HaemaVampireMobsClient.init();
    }
}
