package com.williambl.haema;

import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.hunters.HaemaHunters;
import com.williambl.haema.ritual.HaemaRituals;
import com.williambl.haema.vampire.HaemaVampires;
import com.williambl.haema.vampire_mobs.HaemaVampireMobs;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.itemgroup.v1.FabricItemGroup;
import net.fabricmc.loader.api.FabricLoader;
import net.minecraft.core.Holder;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Haema implements ModInitializer, EntityComponentInitializer {
    public static final Logger LOGGER = LoggerFactory.getLogger("Haema");
    public static final String MODID = "haema";
    public static final Holder.Reference<CreativeModeTab> TAB = Registry.registerForHolder(BuiltInRegistries.CREATIVE_MODE_TAB, id("haema"), FabricItemGroup.builder()
            .icon(() -> HaemaContent.ContentItems.INJECTORS.get(BloodQuality.EXCELLENT).getDefaultInstance())
            .displayItems((itemDisplayParameters, output) -> {
                output.acceptAll(BuiltInRegistries.ITEM.holders().filter(r -> r.key().location().getNamespace().equals(MODID)).map(Holder.Reference::value).map(Item::getDefaultInstance).toList());
            })
                    .title(Component.translatable("itemGroup.haema.haema"))
            .build());

    public static final boolean HOMESTUCK_MODE = FabricLoader.getInstance().isDevelopmentEnvironment();

    public static ResourceLocation id(String path) {
        return new ResourceLocation(MODID, path);
    }

    @Override
    public void onInitialize() {
        HaemaVampires.init();
        HaemaContent.init();
        HaemaRituals.init();
        HaemaHunters.init();
        HaemaVampireMobs.init();
        CommandRegistrationCallback.EVENT.register(HaemaCommand::register);
        HaemaDFunctions.init();
    }

    @Override
    public void registerEntityComponentFactories(EntityComponentFactoryRegistry registry) {
        HaemaVampires.initEntityComponents(registry);
        HaemaVampireMobs.initEntityComponents(registry);
    }
}
