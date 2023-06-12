package com.williambl.haema.content;

import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.content.blood.*;
import com.williambl.haema.content.injector.BloodFillingRecipe;
import com.williambl.haema.content.injector.EmptyInjectorItem;
import com.williambl.haema.content.injector.IncompatibleBloodEffect;
import com.williambl.haema.content.injector.InjectorItem;
import net.fabricmc.fabric.api.transfer.v1.fluid.CauldronFluidContent;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidConstants;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidStorage;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.fluid.base.FullItemFluidStorage;
import net.minecraft.core.Registry;
import net.minecraft.core.cauldron.CauldronInteraction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.item.BucketItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.LayeredCauldronBlock;
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
        Recipes.init();
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

        public static final Map<BloodQuality, Map<Item, CauldronInteraction>> BLOOD_CAULDRON_INTERACTIONS = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        $ -> CauldronInteraction.newInteractionMap())));
        public static final Map<BloodQuality, LayeredCauldronBlock> BLOOD_CAULDRON = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(BuiltInRegistries.BLOCK, id("%s_blood_cauldron".formatted(quality.getSerializedName())),
                                new LayeredCauldronBlock(BlockBehaviour.Properties.copy(Blocks.CAULDRON), $ -> false, BLOOD_CAULDRON_INTERACTIONS.get(quality))))));

        @SuppressWarnings("UnstableApiUsage")
        public static void init() {
            for (var entry : BLOOD_CAULDRON.entrySet()) {
                CauldronFluidContent.registerCauldron(entry.getValue(), BLOOD.get(entry.getKey()), FluidConstants.BOTTLE, LayeredCauldronBlock.LEVEL);
                CauldronInteraction.EMPTY.put(Items.BUCKETS.get(entry.getKey()), (blockState, level, blockPos, player, interactionHand, itemStack) -> CauldronInteraction.emptyBucket(
                        level,
                        blockPos,
                        player,
                        interactionHand,
                        itemStack,
                        entry.getValue().defaultBlockState().setValue(LayeredCauldronBlock.LEVEL, 3),
                        SoundEvents.BUCKET_EMPTY
                ));
                CauldronInteraction.EMPTY.put(Items.BOTTLES.get(entry.getKey()), (blockState, level, blockPos, player, interactionHand, itemStack) -> HaemaUtil.emptyBottle(
                        level,
                        blockPos,
                        player,
                        interactionHand,
                        itemStack,
                        entry.getValue().defaultBlockState().setValue(LayeredCauldronBlock.LEVEL, 1),
                        SoundEvents.BUCKET_EMPTY
                ));
                BLOOD_CAULDRON_INTERACTIONS.values().forEach(map -> map.put(Items.BOTTLES.get(entry.getKey()), (blockState, level, blockPos, player, interactionHand, itemStack) -> CauldronInteraction.emptyBucket(
                        level,
                        blockPos,
                        player,
                        interactionHand,
                        itemStack,
                        entry.getValue().defaultBlockState().setValue(LayeredCauldronBlock.LEVEL, 1),
                        SoundEvents.BUCKET_EMPTY
                )));
                BLOOD_CAULDRON_INTERACTIONS.get(entry.getKey()).put(Items.BOTTLES.get(entry.getKey()), (blockState, level, blockPos, player, interactionHand, itemStack) ->
                        blockState.getValue(LayeredCauldronBlock.LEVEL) >= 3 ? InteractionResult.PASS : CauldronInteraction.emptyBucket(
                                level,
                                blockPos,
                                player,
                                interactionHand,
                                itemStack,
                                blockState.setValue(LayeredCauldronBlock.LEVEL, blockState.getValue(LayeredCauldronBlock.LEVEL) + 1),
                                SoundEvents.BUCKET_EMPTY
                        ));
                BLOOD_CAULDRON_INTERACTIONS.get(entry.getKey()).put(net.minecraft.world.item.Items.GLASS_BOTTLE, (blockState, level, blockPos, player, interactionHand, itemStack) ->
                        HaemaUtil.fillBottle(
                                blockState,
                                level,
                                blockPos,
                                player,
                                interactionHand,
                                itemStack,
                                new ItemStack(Items.BOTTLES.get(entry.getKey())),
                                blockStatex -> true,
                                SoundEvents.BUCKET_EMPTY
                        ));
                BLOOD_CAULDRON_INTERACTIONS.get(entry.getKey()).put(net.minecraft.world.item.Items.BUCKET, (blockState, level, blockPos, player, interactionHand, itemStack) -> CauldronInteraction.fillBucket(
                        blockState,
                        level,
                        blockPos,
                        player,
                        interactionHand,
                        itemStack,
                        new ItemStack(Items.BUCKETS.get(entry.getKey())),
                        blockStatex -> blockStatex.getValue(LayeredCauldronBlock.LEVEL) == 3,
                        SoundEvents.BUCKET_FILL
                ));
            }
        }
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
        public static final Map<BloodQuality, BloodBottleItem> BOTTLES = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(
                                BuiltInRegistries.ITEM,
                                id(quality.getSerializedName() + "_blood_bottle"),
                                new BloodBottleItem(new Item.Properties().stacksTo(1), quality)))));

        //TODO recipe to fill empty injectors with blood

        @SuppressWarnings("UnstableApiUsage")
        public static void init() {
            for (var quality : BloodQuality.values()) {
                FluidStorage.combinedItemApiProvider(INJECTORS.get(quality)).register(ctx -> new FullItemFluidStorage(ctx, EMPTY_INJECTOR, FluidVariant.of(Fluids.BLOOD.get(quality)), Config.INJECTOR_CAPACITY_DROPLETS));
            }
            FluidStorage.combinedItemApiProvider(EMPTY_INJECTOR).register(ctx -> new EmptyBloodStorage(ctx, Config.INJECTOR_CAPACITY_DROPLETS, EMPTY_INJECTOR, INJECTORS::get));

            for (var quality : BloodQuality.values()) {
                FluidStorage.combinedItemApiProvider(BOTTLES.get(quality)).register(ctx -> new FullItemFluidStorage(ctx, net.minecraft.world.item.Items.GLASS_BOTTLE, FluidVariant.of(Fluids.BLOOD.get(quality)), Config.INJECTOR_CAPACITY_DROPLETS));
            }
            FluidStorage.combinedItemApiProvider(net.minecraft.world.item.Items.GLASS_BOTTLE).register(ctx -> new EmptyBloodStorage(ctx, Config.INJECTOR_CAPACITY_DROPLETS, net.minecraft.world.item.Items.GLASS_BOTTLE, BOTTLES::get));
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

    public static class Recipes {
        public static final RecipeSerializer<BloodFillingRecipe> BLOOD_FILLING_SERIALIZER = Registry.register(
                BuiltInRegistries.RECIPE_SERIALIZER,
                id("blood_filling"),
                new BloodFillingRecipe.Serializer());
        public static void init() {}
    }

    @SuppressWarnings("UnstableApiUsage")
    public static class Config {
        public static final long INJECTOR_CAPACITY_DROPLETS = FluidConstants.BOTTLE;
        public static final double INJECTOR_CAPACITY_BLOOD_UNITS = 6.0;
        public static void init() {}
    }
}
