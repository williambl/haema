package com.williambl.haema.content;

import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.api.content.blood.EntityBloodStorageCallback;
import com.williambl.haema.api.content.blood.EntityBloodQualityCallback;
import com.williambl.haema.api.vampire.VampireComponent;
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
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.tags.TagKey;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.BucketItem;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.biome.Biome;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.LayeredCauldronBlock;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.PushReaction;

import java.util.Arrays;
import java.util.EnumMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.williambl.haema.Haema.id;

public class HaemaContent {
    public static void init() {
        ContentFluids.init();
        ContentBlocks.init();
        ContentItems.init();
        ContentVampirismSources.init();
        ContentMobEffects.init();
        ContentRecipes.init();
        ContentConstants.init();
        ContentTags.init();
    }

    public static class ContentFluids {
        public static final Map<BloodQuality, BloodFluid.Flowing> FLOWING_BLOOD = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(BuiltInRegistries.FLUID, id("flowing_%s_blood".formatted(quality.getSerializedName())), new BloodFluid.Flowing(quality)))));
        public static final Map<BloodQuality, BloodFluid.Source> BLOOD = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(BuiltInRegistries.FLUID, id("%s_blood".formatted(quality.getSerializedName())), new BloodFluid.Source(quality)))));

        public static void init() {
            BloodApi.ENTITY_BLOOD_STORAGE.registerFallback((e, $) -> EntityBloodStorageCallback.EVENT.invoker().findStorage(e));
            EntityBloodStorageCallback.EVENT.register(EntityBloodStorageCallback.DEFAULT_HEALTH_BACKED, entity -> {
                if (entity instanceof LivingEntity living) {
                    return BloodApi.getBloodQuality(entity).map(q -> new EntityHealthBackedBloodStorage(living, q))
                            .orElse(null);
                }

                return null;
            });
            EntityBloodStorageCallback.EVENT.register(EntityBloodStorageCallback.VAMPIRE_BLOOD_BACKED, entity -> VampireComponent.KEY.maybeGet(entity)
                    .filter(VampireComponent::isVampire)
                    .flatMap(c -> BloodApi.getBloodQuality(entity).map(q -> new VampireBackedBloodStorage(c, q)))
                    .orElse(null));
            EntityBloodQualityCallback.EVENT.register((entity, original) -> {
                if (VampireComponent.KEY.maybeGet(entity).filter(VampireComponent::isVampire).isPresent()) {
                    return InteractionResultHolder.success(Optional.of(BloodQuality.EXCELLENT));
                }

                return InteractionResultHolder.pass(original);
            });
        }
    }

    public static class ContentBlocks {

        public static final Map<BloodQuality, Block> BLOOD_BLOCK = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(BuiltInRegistries.BLOCK, id("%s_blood".formatted(quality.getSerializedName())), new BloodLiquidBlock(
                                ContentFluids.BLOOD.get(quality), quality, BlockBehaviour.Properties.of().mapColor(DyeColor.RED).replaceable().noCollission().randomTicks().strength(100.0F).pushReaction(PushReaction.DESTROY).noLootTable().liquid().sound(SoundType.EMPTY))))));
        public static final Map<BloodQuality, CauldronInteraction.InteractionMap> BLOOD_CAULDRON_INTERACTIONS = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        b -> CauldronInteraction.newInteractionMap("haema:blood_"+b.getSerializedName()))));
        public static final Map<BloodQuality, LayeredCauldronBlock> BLOOD_CAULDRON = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(BuiltInRegistries.BLOCK, id("%s_blood_cauldron".formatted(quality.getSerializedName())),
                                new LayeredCauldronBlock(Biome.Precipitation.NONE, BLOOD_CAULDRON_INTERACTIONS.get(quality), BlockBehaviour.Properties.ofLegacyCopy(Blocks.CAULDRON))))));

        @SuppressWarnings("UnstableApiUsage")
        public static void init() {
            for (var entry : ContentBlocks.BLOOD_CAULDRON.entrySet()) {
                CauldronFluidContent.registerCauldron(entry.getValue(), ContentFluids.BLOOD.get(entry.getKey()), FluidConstants.BOTTLE, LayeredCauldronBlock.LEVEL);
                CauldronInteraction.EMPTY.map().put(ContentItems.BUCKETS.get(entry.getKey()), (blockState, level, blockPos, player, interactionHand, itemStack) -> CauldronInteraction.emptyBucket(
                        level,
                        blockPos,
                        player,
                        interactionHand,
                        itemStack,
                        entry.getValue().defaultBlockState().setValue(LayeredCauldronBlock.LEVEL, 3),
                        SoundEvents.BUCKET_EMPTY
                ));
                CauldronInteraction.EMPTY.map().put(ContentItems.BOTTLES.get(entry.getKey()), (blockState, level, blockPos, player, interactionHand, itemStack) -> HaemaUtil.emptyBottle(
                        level,
                        blockPos,
                        player,
                        interactionHand,
                        itemStack,
                        entry.getValue().defaultBlockState().setValue(LayeredCauldronBlock.LEVEL, 1),
                        SoundEvents.BUCKET_EMPTY
                ));
                ContentBlocks.BLOOD_CAULDRON_INTERACTIONS.values().forEach(map -> map.map().put(ContentItems.BOTTLES.get(entry.getKey()), (blockState, level, blockPos, player, interactionHand, itemStack) -> CauldronInteraction.emptyBucket(
                        level,
                        blockPos,
                        player,
                        interactionHand,
                        itemStack,
                        entry.getValue().defaultBlockState().setValue(LayeredCauldronBlock.LEVEL, 1),
                        SoundEvents.BUCKET_EMPTY
                )));
                ContentBlocks.BLOOD_CAULDRON_INTERACTIONS.get(entry.getKey()).map().put(ContentItems.BOTTLES.get(entry.getKey()), (blockState, level, blockPos, player, interactionHand, itemStack) ->
                        blockState.getValue(LayeredCauldronBlock.LEVEL) >= 3 ? InteractionResult.PASS : CauldronInteraction.emptyBucket(
                                level,
                                blockPos,
                                player,
                                interactionHand,
                                itemStack,
                                blockState.setValue(LayeredCauldronBlock.LEVEL, blockState.getValue(LayeredCauldronBlock.LEVEL) + 1),
                                SoundEvents.BUCKET_EMPTY
                        ));
                ContentBlocks.BLOOD_CAULDRON_INTERACTIONS.get(entry.getKey()).map().put(net.minecraft.world.item.Items.GLASS_BOTTLE, (blockState, level, blockPos, player, interactionHand, itemStack) ->
                        HaemaUtil.fillBottle(
                                blockState,
                                level,
                                blockPos,
                                player,
                                interactionHand,
                                itemStack,
                                new ItemStack(ContentItems.BOTTLES.get(entry.getKey())),
                                blockStatex -> true,
                                SoundEvents.BUCKET_EMPTY
                        ));
                ContentBlocks.BLOOD_CAULDRON_INTERACTIONS.get(entry.getKey()).map().put(net.minecraft.world.item.Items.BUCKET, (blockState, level, blockPos, player, interactionHand, itemStack) -> CauldronInteraction.fillBucket(
                        blockState,
                        level,
                        blockPos,
                        player,
                        interactionHand,
                        itemStack,
                        new ItemStack(ContentItems.BUCKETS.get(entry.getKey())),
                        blockStatex -> blockStatex.getValue(LayeredCauldronBlock.LEVEL) == 3,
                        SoundEvents.BUCKET_FILL
                ));
            }
        }
    }

    public static class ContentItems {
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
                                new BloodBucketItem(ContentFluids.BLOOD.get(quality), quality, new Item.Properties().stacksTo(1))))));
        public static final Map<BloodQuality, BloodBottleItem> BOTTLES = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        quality -> Registry.register(
                                BuiltInRegistries.ITEM,
                                id(quality.getSerializedName() + "_blood_bottle"),
                                new BloodBottleItem(new Item.Properties().stacksTo(1), quality)))));

        @SuppressWarnings("UnstableApiUsage")
        public static void init() {
            for (var quality : BloodQuality.values()) {
                FluidStorage.combinedItemApiProvider(INJECTORS.get(quality)).register(ctx -> new FullItemFluidStorage(ctx, EMPTY_INJECTOR, FluidVariant.of(ContentFluids.BLOOD.get(quality)), ContentConstants.INJECTOR_CAPACITY_DROPLETS));
            }
            FluidStorage.combinedItemApiProvider(EMPTY_INJECTOR).register(ctx -> new EmptyBloodStorage(ctx, ContentConstants.INJECTOR_CAPACITY_DROPLETS, EMPTY_INJECTOR, INJECTORS::get));

            for (var quality : BloodQuality.values()) {
                FluidStorage.combinedItemApiProvider(BOTTLES.get(quality)).register(ctx -> new FullItemFluidStorage(ctx, net.minecraft.world.item.Items.GLASS_BOTTLE, FluidVariant.of(ContentFluids.BLOOD.get(quality)), ContentConstants.INJECTOR_CAPACITY_DROPLETS));
            }
            FluidStorage.combinedItemApiProvider(net.minecraft.world.item.Items.GLASS_BOTTLE).register(ctx -> new EmptyBloodStorage(ctx, ContentConstants.INJECTOR_CAPACITY_DROPLETS, net.minecraft.world.item.Items.GLASS_BOTTLE, BOTTLES::get));
        }
    }

    public static class ContentVampirismSources {

        public static final ResourceKey<VampirismSource> BLOOD_INJECTOR = ResourceKey.create(VampirismSource.REGISTRY_KEY, id("blood_injector"));

        public static void init() {}
    }

    public static class ContentMobEffects {
        public static final IncompatibleBloodEffect INCOMPATIBLE_BLOOD = Registry.register(
                BuiltInRegistries.MOB_EFFECT,
                id("incompatible_blood"),
                new IncompatibleBloodEffect());

        public static void init() {}
    }

    public static class ContentRecipes {
        public static final RecipeSerializer<BloodFillingRecipe> BLOOD_FILLING_SERIALIZER = Registry.register(
                BuiltInRegistries.RECIPE_SERIALIZER,
                id("blood_filling"),
                new BloodFillingRecipe.Serializer());
        public static void init() {}
    }

    public static class ContentTags {
        public static final Map<BloodQuality, TagKey<Fluid>> BLOOD_TAGS = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        b -> TagKey.create(Registries.FLUID, id(b.getSerializedName()+"_blood"))
                )));

        public static final Map<BloodQuality, TagKey<Fluid>> MINIMUM_QUALITY_BLOOD_TAGS = new EnumMap<>(Arrays.stream(BloodQuality.values())
                .collect(Collectors.toMap(
                        Function.identity(),
                        b -> TagKey.create(Registries.FLUID, id("at_least_"+b.getSerializedName()+"_blood"))
                )));
        public static final Map<BloodQuality, TagKey<EntityType<?>>> ENTITY_BLOOD_QUALITY_TAGS = new EnumMap<>(Arrays.stream(BloodQuality.values()).collect(Collectors.toMap(
                        Function.identity(),
                        quality -> TagKey.create(Registries.ENTITY_TYPE, id("blood_quality/" + quality.getSerializedName())))));

        public static void init() {}
    }

    @SuppressWarnings("UnstableApiUsage")
    public static class ContentConstants {
        public static final long INJECTOR_CAPACITY_DROPLETS = FluidConstants.BOTTLE;
        public static final double BLOOD_UNITS_PER_DROPLET = (double) 20 /*blood units per person*/ / FluidConstants.BUCKET /*blood droplets per person*/;
        public static void init() {}
    }
}
