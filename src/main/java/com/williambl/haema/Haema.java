package com.williambl.haema;

import com.williambl.haema.vampire.HaemaVampires;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.minecraft.resources.ResourceLocation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Haema implements ModInitializer, EntityComponentInitializer {
    public static final Logger LOGGER = LoggerFactory.getLogger("Haema");
    public static final String MODID = "haema";

    public static ResourceLocation id(String path) {
        return new ResourceLocation(MODID, path);
    }

    @Override
    public void onInitialize() {
        HaemaVampires.init();
        CommandRegistrationCallback.EVENT.register(HaemaCommand::register);
    }

    @Override
    public void registerEntityComponentFactories(EntityComponentFactoryRegistry registry) {
        HaemaVampires.initEntityComponents(registry);
    }
}
