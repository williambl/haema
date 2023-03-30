package com.williambl.haema;

import net.fabricmc.api.ModInitializer;
import net.minecraft.resources.ResourceLocation;

public class Haema implements ModInitializer {
    public static final String MODID = "haema";

    public static ResourceLocation id(String path) {
        return new ResourceLocation(MODID, path);
    }

    @Override
    public void onInitialize() {

    }
}
