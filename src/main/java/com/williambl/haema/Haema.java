package com.williambl.haema;

import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodQuality;
import com.williambl.haema.vampire.HaemaVampires;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.itemgroup.v1.FabricItemGroup;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Haema implements ModInitializer, EntityComponentInitializer {
    public static final Logger LOGGER = LoggerFactory.getLogger("Haema");
    public static final String MODID = "haema";
    public static final CreativeModeTab TAB = FabricItemGroup.builder(id("haema"))
            .icon(() -> HaemaContent.Items.INJECTORS.get(BloodQuality.EXCELLENT).getDefaultInstance())
            .displayItems((itemDisplayParameters, output) -> {
                output.acceptAll(BuiltInRegistries.ITEM.holders().filter(r -> r.key().location().getNamespace().equals(MODID)).map(Holder.Reference::value).map(Item::getDefaultInstance).toList());
            })
            .build();

    public static ResourceLocation id(String path) {
        return new ResourceLocation(MODID, path);
    }

    @Override
    public void onInitialize() {
        HaemaVampires.init();
        HaemaContent.init();
        CommandRegistrationCallback.EVENT.register(HaemaCommand::register);
        HaemaDFunctions.init();
    }

    @Override
    public void registerEntityComponentFactories(EntityComponentFactoryRegistry registry) {
        HaemaVampires.initEntityComponents(registry);
    }
}
