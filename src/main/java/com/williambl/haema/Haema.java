package com.williambl.haema;

import com.williambl.haema.vampire.HaemaVampires;
import net.fabricmc.api.ModInitializer;
import net.minecraft.resources.ResourceLocation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Haema implements ModInitializer {
    public static final Logger LOGGER = LoggerFactory.getLogger("Haema");
    public static final String MODID = "haema";

    public static ResourceLocation id(String path) {
        return new ResourceLocation(MODID, path);
    }

    @Override
    public void onInitialize() {
        HaemaVampires.init();
    }
}
