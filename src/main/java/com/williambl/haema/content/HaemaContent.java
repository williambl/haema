package com.williambl.haema.content;

import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.content.injector.EmptyInjectorItem;
import com.williambl.haema.content.injector.InjectorItem;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.LiquidBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.material.Material;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.williambl.haema.Haema.id;

public class HaemaContent {
    public static void init() {
        Fluids.init();
        Items.init();
        VampirismSources.init();
        MobEffects.init();
        Config.init();
    }

    public static class Fluids {
        public static final BloodFluid.Flowing FLOWING_BLOOD = Registry.register(BuiltInRegistries.FLUID, id("flowing_blood"), new BloodFluid.Flowing());
        public static final BloodFluid.Source BLOOD = Registry.register(BuiltInRegistries.FLUID, id("blood"), new BloodFluid.Source());
        public static final Block BLOOD_BLOCK = Registry.register(
                BuiltInRegistries.BLOCK,
                id("blood"),
                new LiquidBlock(
                        Fluids.BLOOD, BlockBehaviour.Properties.of(Material.LAVA).noCollission().randomTicks().strength(100.0F).noLootTable()));

        public static void init() {}
    }

    public static class Items {
        public static final EmptyInjectorItem EMPTY_INJECTOR = Registry.register(
                BuiltInRegistries.ITEM,
                id("empty_injector"),
                new EmptyInjectorItem(new Item.Properties().stacksTo(1)));

        public static final Map<BloodQuality, InjectorItem> INJECTORS = Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(
                                BuiltInRegistries.ITEM,
                                id(quality.name().toLowerCase() + "_injector"),
                                new InjectorItem(new Item.Properties().stacksTo(1), quality))));

        public static void init() {}
    }

    public static class VampirismSources {

        public static final ResourceKey<VampirismSource> BLOOD_INJECTOR = ResourceKey.create(VampirismSource.REGISTRY_KEY, id("blood_injector"));

        public static void init() {}
    }

    public static class MobEffects {
        public static final IncompatibleBloodEffect INCOMPATIBLE_BLOOD = Registry.register(
                BuiltInRegistries.MOB_EFFECT,
                id("incompatible_blood"),
                new IncompatibleBloodEffect());

        public static void init() {}
    }

    public static class Config {
        public static final double INJECTOR_CAPACITY = 6.0;
        public static void init() {}
    }
}
