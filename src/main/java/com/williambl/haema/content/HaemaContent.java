package com.williambl.haema.content;

import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
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
}
