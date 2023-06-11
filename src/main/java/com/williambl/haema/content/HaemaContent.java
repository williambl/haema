package com.williambl.haema.content;

import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.content.blood.BloodBucketItem;
import com.williambl.haema.content.blood.BloodFluid;
import com.williambl.haema.content.blood.BloodLiquidBlock;
import com.williambl.haema.content.blood.BloodQuality;
import com.williambl.haema.content.injector.EmptyInjectorItem;
import com.williambl.haema.content.injector.EmptyInjectorStorage;
import com.williambl.haema.content.injector.IncompatibleBloodEffect;
import com.williambl.haema.content.injector.InjectorItem;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidConstants;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidStorage;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.fluid.base.FullItemFluidStorage;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.BucketItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.material.Material;

import java.util.Arrays;
import java.util.EnumMap;
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
        public static final Map<BloodQuality, BloodFluid.Flowing> FLOWING_BLOOD = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(BuiltInRegistries.FLUID, id("flowing_%s_blood".formatted(quality.getSerializedName())), new BloodFluid.Flowing(quality)))));
        public static final Map<BloodQuality, BloodFluid.Source> BLOOD = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(BuiltInRegistries.FLUID, id("%s_blood".formatted(quality.getSerializedName())), new BloodFluid.Source(quality)))));
        public static final Map<BloodQuality, Block> BLOOD_BLOCK = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(BuiltInRegistries.BLOCK, id("%s_blood".formatted(quality.getSerializedName())), new BloodLiquidBlock(
                                BLOOD.get(quality), quality, BlockBehaviour.Properties.of(Material.LAVA).noCollission().randomTicks().strength(100.0F).noLootTable())))));

        public static void init() {}
    }

    public static class Items {
        public static final EmptyInjectorItem EMPTY_INJECTOR = Registry.register(
                BuiltInRegistries.ITEM,
                id("empty_injector"),
                new EmptyInjectorItem(new Item.Properties().stacksTo(1)));

        public static final Map<BloodQuality, InjectorItem> INJECTORS = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(
                                BuiltInRegistries.ITEM,
                                id(quality.getSerializedName() + "_injector"),
                                new InjectorItem(new Item.Properties().stacksTo(1), quality)))));
        public static final Map<BloodQuality, BucketItem> BUCKETS = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(
                                BuiltInRegistries.ITEM,
                                id(quality.getSerializedName() + "_blood_bucket"),
                                new BloodBucketItem(Fluids.BLOOD.get(quality), quality, new Item.Properties().stacksTo(1))))));

        @SuppressWarnings("UnstableApiUsage")
        public static void init() {
            for (var quality : BloodQuality.values()) {
                FluidStorage.combinedItemApiProvider(INJECTORS.get(quality)).register(ctx -> new FullItemFluidStorage(ctx, EMPTY_INJECTOR, FluidVariant.of(Fluids.BLOOD.get(quality)), Config.INJECTOR_CAPACITY_DROPLETS));
            }
            FluidStorage.combinedItemApiProvider(EMPTY_INJECTOR).register(EmptyInjectorStorage::new);
        }
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

    @SuppressWarnings("UnstableApiUsage")
    public static class Config { //TODO make this configurable somehow
        public static final double INJECTOR_CAPACITY_BLOOD_UNITS = 6.0;
        public static final long DROPLETS_PER_BLOOD_UNIT = FluidConstants.BUCKET * 2 / 20; // one player holds two buckets of blood
        public static final long INJECTOR_CAPACITY_DROPLETS = (long) (INJECTOR_CAPACITY_BLOOD_UNITS * DROPLETS_PER_BLOOD_UNIT);
        public static void init() {}
    }
}
