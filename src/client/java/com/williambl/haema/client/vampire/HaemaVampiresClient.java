package com.williambl.haema.client.vampire;

import com.williambl.haema.client.vampire.ability.powers.vision.VampireVisionFx;

public class HaemaVampiresClient {
    public static void init() {
        new VampireVisionFx().init();
    }
}
