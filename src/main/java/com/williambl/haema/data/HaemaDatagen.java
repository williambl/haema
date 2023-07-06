package com.williambl.haema.data;

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes;
import com.williambl.dfunc.api.Comparison;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.ContextArg;
import com.williambl.dfunc.api.functions.*;
import com.williambl.haema.Haema;
import com.williambl.haema.HaemaCommand;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.api.ritual.MultiblockFilter;
import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.api.ritual.ritual.Ritual;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodBottleItem;
import com.williambl.haema.content.injector.BloodFillingRecipe;
import com.williambl.haema.content.injector.InjectorItem;
import com.williambl.haema.hunters.HaemaHunters;
import com.williambl.haema.hunters.VampireHunter;
import com.williambl.haema.hunters.VampireHunterContractItem;
import com.williambl.haema.ritual.HaemaRituals;
import com.williambl.haema.ritual.module.ParticlesToCentreAraeModule;
import com.williambl.haema.ritual.ritual.RemoveUsedItemsRitualAction;
import com.williambl.haema.ritual.ritual.RightClickRitualTrigger;
import com.williambl.haema.ritual.ritual.SetFluidRitualAction;
import com.williambl.haema.ritual.ritual.SpawnEntityRitualAction;
import com.williambl.haema.vampire.HaemaVampires;
import com.williambl.haema.vampire.ability.powers.AttributeVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.EffectVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.HealingVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.damage_modification.DamageModificationAbilityPower;
import com.williambl.haema.vampire.ability.powers.vision.VampireVisionVampireAbilityPower;
import net.fabricmc.fabric.api.datagen.v1.DataGeneratorEntrypoint;
import net.fabricmc.fabric.api.datagen.v1.FabricDataGenerator;
import net.fabricmc.fabric.api.datagen.v1.FabricDataOutput;
import net.fabricmc.fabric.api.datagen.v1.provider.*;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.RegistrySetBuilder;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.models.BlockModelGenerators;
import net.minecraft.data.models.ItemModelGenerators;
import net.minecraft.data.models.blockstates.MultiVariantGenerator;
import net.minecraft.data.models.blockstates.PropertyDispatch;
import net.minecraft.data.models.blockstates.Variant;
import net.minecraft.data.models.blockstates.VariantProperties;
import net.minecraft.data.models.model.*;
import net.minecraft.data.recipes.FinishedRecipe;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.BlockTags;
import net.minecraft.tags.DamageTypeTags;
import net.minecraft.world.damagesource.DamageType;
import net.minecraft.world.damagesource.DamageTypes;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.enchantment.Enchantment;
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.level.ItemLike;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.LayeredCauldronBlock;
import net.minecraft.world.level.levelgen.blockpredicates.BlockPredicate;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.LootPool;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.entries.LootItem;
import net.minecraft.world.level.storage.loot.entries.LootPoolEntryContainer;
import net.minecraft.world.level.storage.loot.functions.EnchantRandomlyFunction;
import net.minecraft.world.level.storage.loot.functions.SetItemCountFunction;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.minecraft.world.level.storage.loot.predicates.LootItemCondition;
import net.minecraft.world.level.storage.loot.providers.number.UniformGenerator;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import static com.williambl.haema.Haema.id;

public class HaemaDatagen implements DataGeneratorEntrypoint {
    @Override
    public void onInitializeDataGenerator(FabricDataGenerator fabricDataGenerator) {
        var pack = fabricDataGenerator.createPack();
        pack.addProvider(HaemaDynamicRegistryProvider::new);
        pack.addProvider(HaemaBlockTagsProvider::new);
        pack.addProvider(HaemaFluidTagsProvider::new);
        pack.addProvider((o, r) -> new HaemaItemTagsProvider(o, r, null));
        pack.addProvider(HaemaEntityTagsProvider::new);
        pack.addProvider(HaemaDamageTypeTagsProvider::new);
        pack.addProvider(HaemaLangProvider::new);
        pack.addProvider(HaemaModelProvider::new);
        pack.addProvider(HaemaRecipeProvider::new);
        pack.addProvider(HaemaBarterLootProvider::new);
    }

    @Override
    public @Nullable String getEffectiveModId() {
        return Haema.MODID;
    }

    @Override
    public void buildRegistry(RegistrySetBuilder registryBuilder) {
        registryBuilder.add(VampirismSource.REGISTRY_KEY, $ -> {});
        registryBuilder.add(VampireAbility.REGISTRY_KEY, $ -> {});
        registryBuilder.add(RitualArae.REGISTRY_KEY, $ -> {});
        registryBuilder.add(Ritual.REGISTRY_KEY, $ -> {});
    }

    private static class HaemaModelProvider extends FabricModelProvider {

        public HaemaModelProvider(FabricDataOutput output) {
            super(output);
        }

        @Override
        public void generateBlockStateModels(BlockModelGenerators models) {
            for (var entry : HaemaContent.ContentBlocks.BLOOD_CAULDRON.entrySet()) {
                var cauldron = entry.getValue();
                models.blockStateOutput.accept(
                        MultiVariantGenerator.multiVariant(cauldron).with(
                                PropertyDispatch.property(LayeredCauldronBlock.LEVEL).select(
                                                1,
                                                Variant.variant().with(
                                                        VariantProperties.MODEL,
                                                        ModelTemplates.CAULDRON_LEVEL1.createWithSuffix(
                                                                cauldron, "_level1", TextureMapping.cauldron(TextureMapping.getBlockTexture(Blocks.WATER, "_still")), models.modelOutput)))
                                        .select(
                                                2,
                                                Variant.variant()
                                                        .with(
                                                                VariantProperties.MODEL,
                                                                ModelTemplates.CAULDRON_LEVEL2
                                                                        .createWithSuffix(
                                                                                cauldron, "_level2", TextureMapping.cauldron(TextureMapping.getBlockTexture(Blocks.WATER, "_still")), models.modelOutput)))
                                        .select(
                                                3,
                                                Variant.variant()
                                                        .with(
                                                                VariantProperties.MODEL,
                                                                ModelTemplates.CAULDRON_FULL
                                                                        .createWithSuffix(cauldron, "_full", TextureMapping.cauldron(TextureMapping.getBlockTexture(Blocks.WATER, "_still")), models.modelOutput)))));
            }
        }

        @Override
        public void generateItemModels(ItemModelGenerators models) {
            models.generateFlatItem(HaemaContent.ContentItems.EMPTY_INJECTOR, ModelTemplates.FLAT_ITEM);
            for (var item : HaemaContent.ContentItems.INJECTORS.values()) {
                this.generateFlatItem(item, id("item/full_injector"), ModelTemplates.FLAT_ITEM, models);
            }
            for (var item : HaemaContent.ContentItems.BUCKETS.values()) {
                models.generateFlatItem(item, Items.WATER_BUCKET, ModelTemplates.FLAT_ITEM); //TODO use own texture
            }
            for (var item : HaemaContent.ContentItems.BOTTLES.values()) {
                this.generateFlatItem(item, id("item/blood_bottle"), ModelTemplates.FLAT_ITEM, models);
            }
            this.generateOverlayedItem(HaemaHunters.HunterItems.VAMPIRE_HUNTER_CONTRACT, new ResourceLocation("item/mojang_banner_pattern"), id("item/contract_overlay"), ModelTemplates.TWO_LAYERED_ITEM, models);
        }

        public final void generateFlatItem(Item item, ResourceLocation texture, ModelTemplate modelTemplate, ItemModelGenerators models) {
            modelTemplate.create(ModelLocationUtils.getModelLocation(item), TextureMapping.layer0(texture), models.output);
        }

        public final void generateOverlayedItem(Item item, ResourceLocation baseTexture, ResourceLocation texture, ModelTemplate modelTemplate, ItemModelGenerators models) {
            modelTemplate.create(ModelLocationUtils.getModelLocation(item), TextureMapping.layer0(baseTexture).put(TextureSlot.LAYER1, texture), models.output);
        }
    }

    private static class HaemaItemTagsProvider extends FabricTagProvider.ItemTagProvider {
        public HaemaItemTagsProvider(FabricDataOutput output, CompletableFuture<HolderLookup.Provider> completableFuture, @Nullable BlockTagProvider blockTagProvider) {
            super(output, completableFuture, blockTagProvider);
        }

        @Override
        protected void addTags(HolderLookup.Provider arg) {
            this.tag(HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_WEAPONS).add(BuiltInRegistries.ITEM.getResourceKey(Items.WOODEN_SWORD).orElseThrow());
        }
    }

    private static class HaemaBlockTagsProvider extends FabricTagProvider.BlockTagProvider {

        public HaemaBlockTagsProvider(FabricDataOutput output, CompletableFuture<HolderLookup.Provider> registriesFuture) {
            super(output, registriesFuture);
        }

        @Override
        protected void addTags(HolderLookup.Provider arg) {
            var cauldrons = this.getOrCreateTagBuilder(BlockTags.CAULDRONS);
            for (var cauldron : HaemaContent.ContentBlocks.BLOOD_CAULDRON.values()) {
                cauldrons.add(cauldron);
            }
        }
    }

    private static class HaemaFluidTagsProvider extends FabricTagProvider.FluidTagProvider {

        public HaemaFluidTagsProvider(FabricDataOutput output, CompletableFuture<HolderLookup.Provider> completableFuture) {
            super(output, completableFuture);
        }

        @Override
        protected void addTags(HolderLookup.Provider arg) {
            for (var quality : HaemaContent.ContentTags.BLOOD_TAGS.entrySet()) {
                this.getOrCreateTagBuilder(quality.getValue())
                        .add(BloodApi.getFluid(quality.getKey()))
                        .add(HaemaContent.ContentFluids.FLOWING_BLOOD.get(quality.getKey()));
            }
            BloodQuality[] qualities = BloodQuality.values();
            for (int i = 0; i < qualities.length; i++) {
                var tagBuilder = this.getOrCreateTagBuilder(HaemaContent.ContentTags.MINIMUM_QUALITY_BLOOD_TAGS.get(qualities[i]));
                for (int j = qualities.length-1; j >= i; j--) {
                    tagBuilder.addTag(BloodApi.getFluidTag(qualities[j]));
                }
            }
        }
    }

    private static class HaemaEntityTagsProvider extends FabricTagProvider.EntityTypeTagProvider {

        public HaemaEntityTagsProvider(FabricDataOutput output, CompletableFuture<HolderLookup.Provider> completableFuture) {
            super(output, completableFuture);
        }

        @Override
        protected void addTags(HolderLookup.Provider arg) {
            this.getOrCreateTagBuilder(BloodApi.getEntityTag(BloodQuality.GOOD))
                    .add(EntityType.PLAYER);
        }
    }

    private static class HaemaDamageTypeTagsProvider extends FabricTagProvider<DamageType> {

        public HaemaDamageTypeTagsProvider(FabricDataOutput output, CompletableFuture<HolderLookup.Provider> registriesFuture) {
            super(output, Registries.DAMAGE_TYPE, registriesFuture);
        }

        @Override
        protected void addTags(HolderLookup.Provider arg) {
            this.getOrCreateTagBuilder(HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_DAMAGE)
                    .add(DamageTypes.MAGIC)
                    .forceAddTag(DamageTypeTags.BYPASSES_INVULNERABILITY)
                    .forceAddTag(DamageTypeTags.BYPASSES_EFFECTS)
                    .forceAddTag(DamageTypeTags.BYPASSES_RESISTANCE)
                    .forceAddTag(DamageTypeTags.IS_LIGHTNING)
                    .forceAddTag(DamageTypeTags.IS_DROWNING);
        }
    }

    @SuppressWarnings("UnstableApiUsage")
    private static class HaemaDynamicRegistryProvider extends FabricDynamicRegistryProvider {

        public HaemaDynamicRegistryProvider(FabricDataOutput output, CompletableFuture<HolderLookup.Provider> registriesFuture) {
            super(output, registriesFuture);
        }

        @Override
        protected void configure(HolderLookup.Provider registries, Entries entries) {
            var defaultAbilties = new HashSet<ResourceKey<VampireAbility>>();
            var healingAbility = this.createHealingAbility(entries);
            defaultAbilties.add(healingAbility);
            var reachAbility = this.createReachAbility(entries);
            defaultAbilties.add(reachAbility);
            var healthBoostAbility = this.createHealthBoostAbility(entries);
            defaultAbilties.add(healthBoostAbility);
            var sunlightSicknessAbility = this.createSunlightSicknessAbility(entries);
            defaultAbilties.add(sunlightSicknessAbility);
            var vampiricWeaknessAbility = this.createVampiricWeaknessAbility(entries);
            defaultAbilties.add(vampiricWeaknessAbility);
            var damageModificationAbility = this.createDamageModificationAbility(entries);
            defaultAbilties.add(damageModificationAbility);
            var vampireVisionAbility = this.createVampireVisionAbility(entries);
            defaultAbilties.add(vampireVisionAbility);
            var vampiricStrengthAbilities = new ArrayList<ResourceKey<VampireAbility>>();
            for (int i = 0; i < 3; i++) {
                vampiricStrengthAbilities.add(this.createStrengthAbility(entries, i+1, vampiricStrengthAbilities));
            }
            defaultAbilties.addAll(vampiricStrengthAbilities);


            entries.add(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR, new VampirismSource(Set.of(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR, HaemaVampires.VampirismSources.COMMAND), defaultAbilties, DPredicates.CONSTANT.factory().apply(false), DPredicates.CONSTANT.factory().apply(false))); //TODO
            entries.add(HaemaVampires.VampirismSources.COMMAND, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of(), DPredicates.CONSTANT.factory().apply(true), DPredicates.CONSTANT.factory().apply(true)));

            entries.add(ResourceKey.create(RitualArae.REGISTRY_KEY, id("basic")), new RitualArae(new MultiblockFilter(
                    new char[][][]{
                            new char[][] {
                                    "ccccc".toCharArray(),
                                    "ccccc".toCharArray(),
                                    "ccccc".toCharArray(),
                                    "ccccc".toCharArray(),
                                    "ccccc".toCharArray()
                            },
                            new char[][] {
                                    " CCC ".toCharArray(),
                                    "C   C".toCharArray(),
                                    "C r C".toCharArray(),
                                    "C   C".toCharArray(),
                                    " CCC ".toCharArray()
                            },
                            new char[][] {
                                    "     ".toCharArray(),
                                    "     ".toCharArray(),
                                    "     ".toCharArray(),
                                    "     ".toCharArray(),
                                    "     ".toCharArray()
                            }
                    },
                    'r',
                    Map.of(
                            'c', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(Blocks.CRIMSON_PLANKS), ContextArg.BLOCK.arg()),
                            'C', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(Blocks.CANDLE), ContextArg.BLOCK.arg()),
                            'r', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(HaemaRituals.RitualBlocks.RITUAL_ALTAR), ContextArg.BLOCK.arg())),
                    DPredicates.CONSTANT.factory().apply(true)
            ),
                    Set.of(' '),
                    List.of(new ParticlesToCentreAraeModule(20, ParticleTypes.FLAME, List.of(
                    new Vec3(-0.5, 0.5, 2.5),
                    new Vec3(0.5, 0.5, 2.5),
                    new Vec3(1.5, 0.5, 2.5),
                    new Vec3(-1.5, 0.5, 1.5),
                    new Vec3(2.5, 0.5, 1.5),
                    new Vec3(-1.5, 0.5, 0.5),
                    new Vec3(2.5, 0.5, 0.5),
                    new Vec3(-1.5, 0.5, -0.5),
                    new Vec3(2.5, 0.5, -0.5),
                    new Vec3(-1.5, 0.5, -1.5),
                    new Vec3(0.5, 0.5, -1.5),
                    new Vec3(1.5, 0.5, -1.5)),
                            new Vec3(0.5, 0.5, 0.5), 0.05, 0.2))));

            var bloodAltar = entries.add(ResourceKey.create(RitualArae.REGISTRY_KEY, id("blood")), new RitualArae(new MultiblockFilter(
                    new char[][][]{
                            new char[][] {
                                    " ccccc ".toCharArray(),
                                    "c_____c".toCharArray(),
                                    "c_ccc_c".toCharArray(),
                                    "c_crc_c".toCharArray(),
                                    "c_ccc_c".toCharArray(),
                                    "c_____c".toCharArray(),
                                    " ccccc ".toCharArray()
                            },
                            new char[][] {
                                    "C     C".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "C     C".toCharArray()
                            },
                            new char[][] {
                                    "C     C".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "C     C".toCharArray()
                            },
                            new char[][] {
                                    "C     C".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "C     C".toCharArray()
                            },
                            new char[][] {
                                    "F     F".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "F     F".toCharArray()
                            },
                    },
                    'r',
                    Map.of(
                            'c', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(Blocks.CRIMSON_NYLIUM), ContextArg.BLOCK.arg()),
                            'C', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(Blocks.CRIMSON_STEM), ContextArg.BLOCK.arg()),
                            //TODO this doesn't work due to a null-handling bug in jsonops i think? hmmm
                            //'C', BlockInWorldDFunctions.ADVANCEMENT_PREDICATE.factory().apply(net.minecraft.advancements.critereon.BlockPredicate.Builder.block().of(Blocks.CRIMSON_STEM).setProperties(StatePropertiesPredicate.Builder.properties().hasProperty(BlockStateProperties.AXIS, Direction.Axis.Y).build()).build(), ContextArg.BLOCK.arg()),
                            'r', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(HaemaRituals.RitualBlocks.RITUAL_ALTAR), ContextArg.BLOCK.arg()),
                            'F', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(Blocks.SOUL_CAMPFIRE), ContextArg.BLOCK.arg()),
                            //'F', BlockInWorldDFunctions.ADVANCEMENT_PREDICATE.factory().apply(net.minecraft.advancements.critereon.BlockPredicate.Builder.block().of(Blocks.SOUL_CAMPFIRE).setProperties(StatePropertiesPredicate.Builder.properties().hasProperty(BlockStateProperties.LIT, true).build()).build(), ContextArg.BLOCK.arg()),
                            '_', DPredicates.CONSTANT.factory().apply(true)
                            ),
                    DPredicates.CONSTANT.factory().apply(true)
            ),
                    Set.of('_'),
                    List.of(new ParticlesToCentreAraeModule(60, ParticleTypes.SOUL_FIRE_FLAME, List.of(
                            new Vec3(3.5, 4.25, 3.5),
                            new Vec3(-2.5, 4.25, 3.5),
                            new Vec3(-2.5, 4.25, -2.5),
                            new Vec3(3.5, 4.25, -2.5)),
                            new Vec3(0.5, 0.5, 0.5), 0.05, 0.2))));

            Ritual.Builder.ritual(registries)
                    .fluid(BloodApi.getFluidTagMinimumQuality(BloodQuality.MIDDLING))
                    .acceptableAraes(bloodAltar.unwrapKey().orElseThrow())
                    .trigger(new RightClickRitualTrigger(DPredicates.CONSTANT.factory().apply(true)))
                    .ingredient(Ingredient.of(Items.PORKCHOP))
                    .ingredient(Ingredient.of(Items.PORKCHOP))
                    .ingredient(Ingredient.of(Items.PORKCHOP))
                    .ingredient(Ingredient.of(Items.PORKCHOP))
                    .ingredient(Ingredient.of(Items.PORKCHOP))
                    .action(new SpawnEntityRitualAction(EntityType.PIG))
                    .action(new RemoveUsedItemsRitualAction())
                    .action(new SetFluidRitualAction(Fluids.EMPTY))
                    .build(entries::add, id("reanimate_pig"));
        }

        private ResourceKey<VampireAbility> createHealingAbility(Entries entries) {
            var healingAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(
                    new HealingVampireAbilityPower(DPredicates.AND.factory().apply(List.of(
                            LevelDFunctions.BOOLEAN_GAME_RULE.factory().apply("naturalRegeneration", ContextArg.LEVEL.arg()),
                            DPredicates.NOT.factory().apply(EntityDFunctions.DEAD_OR_DYING.factory().apply(ContextArg.ENTITY.arg())),
                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(NumberDFunctions.MODULO.factory().apply(ContextArg.NUMBER_A.arg(EntityDFunctions.AGE.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(20.0)))), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(0.)), Comparison.EQUAL),
                            DPredicates.NOT.factory().apply(EntityDFunctions.HAS_EFFECT.factory().apply(HaemaVampires.VampireMobEffects.SUNLIGHT_SICKNESS, ContextArg.ENTITY.arg())),
                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(EntityDFunctions.HEALTH.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(EntityDFunctions.ATTRIBUTE.factory().apply(Attributes.MAX_HEALTH, ContextArg.ENTITY.arg())), Comparison.LESS_THAN),
                            DPredicates.OR.factory().apply(List.of(
                                    DPredicates.AND.factory().apply(List.of(
                                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(19.0)), Comparison.GREATER_THAN_OR_EQUAL),
                                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(NumberDFunctions.SUBTRACT.factory().apply(ContextArg.NUMBER_A.arg(EntityDFunctions.HEALTH.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(EntityDFunctions.ATTRIBUTE_BASE.factory().apply(Attributes.MAX_HEALTH, ContextArg.ENTITY.arg())))), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(20.0)), Comparison.LESS_THAN)
                                    )),
                                    DPredicates.AND.factory().apply(List.of(
                                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(14.0)), Comparison.GREATER_THAN_OR_EQUAL),
                                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(NumberDFunctions.SUBTRACT.factory().apply(ContextArg.NUMBER_A.arg(EntityDFunctions.HEALTH.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(EntityDFunctions.ATTRIBUTE_BASE.factory().apply(Attributes.MAX_HEALTH, ContextArg.ENTITY.arg())))), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(10.0)), Comparison.LESS_THAN)
                                    )),
                                    DPredicates.AND.factory().apply(List.of(
                                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(10.0)), Comparison.GREATER_THAN_OR_EQUAL),
                                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(NumberDFunctions.SUBTRACT.factory().apply(ContextArg.NUMBER_A.arg(EntityDFunctions.HEALTH.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(EntityDFunctions.ATTRIBUTE_BASE.factory().apply(Attributes.MAX_HEALTH, ContextArg.ENTITY.arg())))), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(6.0)), Comparison.LESS_THAN)
                                    )),
                                    DPredicates.AND.factory().apply(List.of(
                                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(8.0)), Comparison.GREATER_THAN_OR_EQUAL),
                                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(NumberDFunctions.SUBTRACT.factory().apply(ContextArg.NUMBER_A.arg(EntityDFunctions.HEALTH.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(EntityDFunctions.ATTRIBUTE_BASE.factory().apply(Attributes.MAX_HEALTH, ContextArg.ENTITY.arg())))), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(0.0)), Comparison.LESS_THAN)
                                    )))))),
                            NumberDFunctions.CONSTANT.factory().apply(1.0),
                            NumberDFunctions.POLYNOMIAL.factory().apply(List.of(1.05, 0.0, -1.0), ContextArg.NUMBER_A.arg(NumberDFunctions.DIVIDE.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(HaemaDFunctions.MAX_BLOOD.factory().apply(ContextArg.ENTITY.arg()))))))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("healing"));
            entries.add(key, healingAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createReachAbility(Entries entries) {
            var reachAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(new AttributeVampireAbilityPower(Set.of(
                    new AttributeVampireAbilityPower.Data(
                            ReachEntityAttributes.REACH,
                            new AttributeModifier(UUID.fromString("0eb4fc5f-71d5-4440-b517-bcc18e1df6f4"), "Vampire Reach bonus", 2.0, AttributeModifier.Operation.ADDITION),
                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(6.0)), Comparison.GREATER_THAN_OR_EQUAL)
                    ),
                    new AttributeVampireAbilityPower.Data(
                            ReachEntityAttributes.ATTACK_RANGE,
                            new AttributeModifier(UUID.fromString("3267a46b-2b48-429f-a3a8-439aa87a876d"), "Vampire Attack Range bonus", 2.0, AttributeModifier.Operation.ADDITION),
                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(6.0)), Comparison.GREATER_THAN_OR_EQUAL)
                    )
            ))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("reach"));
            entries.add(key, reachAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createHealthBoostAbility(Entries entries) {
            var healthBoostAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(new AttributeVampireAbilityPower(Set.of(
                    new AttributeVampireAbilityPower.Data(
                            Attributes.MAX_HEALTH,
                            new AttributeModifier(UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92"), "Vampire Max Health bonus", 1.0, AttributeModifier.Operation.MULTIPLY_BASE),
                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(6.0)), Comparison.GREATER_THAN_OR_EQUAL)
                    )
            ))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("health_boost"));
            entries.add(key, healthBoostAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createSunlightSicknessAbility(Entries entries) {
            var sunlightSicknessAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(new EffectVampireAbilityPower(Set.of(
                    new EffectVampireAbilityPower.Data(
                            HaemaVampires.VampireMobEffects.SUNLIGHT_SICKNESS,
                            NumberDFunctions.CONSTANT.factory().apply(0.0),
                            NumberDFunctions.CONSTANT.factory().apply(10.0),
                            DPredicates.CONSTANT.factory().apply(false),
                            DPredicates.CONSTANT.factory().apply(true),
                            DPredicates.CONSTANT.factory().apply(true),
                            DPredicates.AND.factory().apply(List.of(
                                    LevelDFunctions.BOOLEAN_GAME_RULE.factory().apply(HaemaVampires.VampireGameRules.VAMPIRES_BURN.getId(), ContextArg.LEVEL.arg()),
                                    EntityDFunctions.IS_SURVIVAL_LIKE.factory().apply(ContextArg.ENTITY.arg()),
                                    DPredicates.OR.factory().apply(List.of(
                                            DPredicates.AND.factory().apply(List.of(
                                                    EntityDFunctions.CAN_SEE_SKY.factory().apply(ContextArg.ENTITY.arg()),
                                                    LevelDFunctions.IS_DAY.factory().apply(ContextArg.LEVEL.arg()),
                                                    DPredicates.NOT.factory().apply(LevelDFunctions.IS_RAINING.factory().apply(ContextArg.LEVEL.arg())))),
                                            HaemaDFunctions.TRIGGER_BURN_EVENT.factory().apply(ContextArg.ENTITY.arg()))),
                                    DPredicates.NOT.factory().apply(HaemaDFunctions.PREVENT_BURN_EVENT.factory().apply(ContextArg.ENTITY.arg())))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("sunlight_sickness"));
            entries.add(key, sunlightSicknessAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createVampiricWeaknessAbility(Entries entries) {
            var vampiricWeaknessAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(new EffectVampireAbilityPower(Set.of(
                    new EffectVampireAbilityPower.Data(
                            HaemaVampires.VampireMobEffects.VAMPIRIC_WEAKNESS,
                            NumberDFunctions.SUBTRACT.factory().apply(ContextArg.NUMBER_A.arg(NumberDFunctions.CONSTANT.factory().apply(3.0)), ContextArg.NUMBER_B.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg()))),
                            NumberDFunctions.CONSTANT.factory().apply(5.0),
                            DPredicates.CONSTANT.factory().apply(false),
                            DPredicates.CONSTANT.factory().apply(true),
                            DPredicates.CONSTANT.factory().apply(true),
                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(3.0)), Comparison.LESS_THAN_OR_EQUAL))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("vampiric_weakness"));
            entries.add(key, vampiricWeaknessAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createDamageModificationAbility(Entries entries) {
            var damageModificationAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(false), Set.of(),Set.of(), Set.of(), List.of(new DamageModificationAbilityPower(
                    NumberDFunctions.MULTIPLY.factory().apply(
                            ContextArg.NUMBER_A.arg("damage_amount"),
                            ContextArg.NUMBER_B.arg(NumberDFunctions.MAX.factory().apply(ContextArg.NUMBER_A.arg(
                                            NumberDFunctions.IF_ELSE.factory().apply(
                                                    ContextArg.NUMBER_A.arg(NumberDFunctions.CONSTANT.factory().apply(1.25)),
                                                    ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(1.0)),
                                                    DPredicates.OR.factory().apply(List.of(
                                                            DamageSourceDFunctions.DAMAGE_TAG.factory().apply(HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_DAMAGE, ContextArg.DAMAGE_SOURCE.arg()),
                                                            ItemStackDFunctions.ITEM_TAG.factory().apply(HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_WEAPONS, ContextArg.ITEM.arg("weapon")))))),
                                    ContextArg.NUMBER_B.arg(ItemStackDFunctions.ENCHANTMENT_LEVEL.factory().apply(Enchantments.SMITE, ContextArg.ITEM.arg("weapon")))))),
                    DPredicates.OR.factory().apply(List.of(NumberDFunctions.COMPARISON.factory().apply(
                            ContextArg.NUMBER_A.arg(ItemStackDFunctions.ENCHANTMENT_LEVEL.factory().apply(Enchantments.SMITE, ContextArg.ITEM.arg("weapon"))),
                            ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(0.0)),
                            Comparison.GREATER_THAN),
                            DamageSourceDFunctions.DAMAGE_TAG.factory().apply(HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_DAMAGE, ContextArg.DAMAGE_SOURCE.arg()),
                            ItemStackDFunctions.ITEM_TAG.factory().apply(HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_WEAPONS, ContextArg.ITEM.arg("weapon")))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("damage_modification"));
            entries.add(key, damageModificationAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createVampireVisionAbility(Entries entries) {
            var vampireVisionAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(
                    new VampireVisionVampireAbilityPower()));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("vampire_vision"));
            entries.add(key, vampireVisionAbility);
            return key;
        }


        private ResourceKey<VampireAbility> createStrengthAbility(Entries entries, int level, Collection<ResourceKey<VampireAbility>> before) {
            var match = new HashMap<DFunction<Boolean>, ContextArg<Double>>();
            class Util {
                void addMatch(double blood, double amplifier) {
                    match.put(NumberDFunctions.COMPARISON.factory().apply(
                                    ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())),
                                    ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(blood)),
                                    Comparison.GREATER_THAN_OR_EQUAL),
                            ContextArg.NUMBER_A.arg(NumberDFunctions.CONSTANT.factory().apply(amplifier)));
                }
            }
            var util = new Util();
            if (level >= 2) {
                util.addMatch(14.0, 1.0);
            }
            if (level >= 3) {
                util.addMatch(19.0, 2.0);
            }
            var strengthAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(true), Set.copyOf(before), Set.of(), Set.copyOf(before), List.of(
                    new EffectVampireAbilityPower(Set.of(
                            new EffectVampireAbilityPower.Data(
                                    HaemaVampires.VampireMobEffects.VAMPIRIC_STRENGTH,
                                    NumberDFunctions.MATCH.factory().apply(match, ContextArg.NUMBER_A.arg(NumberDFunctions.CONSTANT.factory().apply(0.0))),
                                    NumberDFunctions.CONSTANT.factory().apply(40.0),
                                    DPredicates.CONSTANT.factory().apply(false),
                                    DPredicates.CONSTANT.factory().apply(true),
                                    DPredicates.CONSTANT.factory().apply(true),
                                    NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(HaemaDFunctions.BLOOD.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(10.0)), Comparison.GREATER_THAN_OR_EQUAL))))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("strength/"+level));
            entries.add(key, strengthAbility);
            return key;
        }

        @Override
        public String getName() {
            return "Dynamic Registry Objects";
        }
    }

    public static class HaemaLangProvider extends FabricLanguageProvider {
        protected HaemaLangProvider(FabricDataOutput dataOutput) {
            super(dataOutput);
        }

        @Override
        public void generateTranslations(TranslationBuilder translations) {
            translations.add(HaemaCommand.NO_SUCH_VAMPIRISM_SOURCE_KEY, "No such vampirism source %s");
            translations.add(HaemaCommand.NOT_VAMPIRABLE_KEY, "%s cannot be a vampire");
            translations.add(HaemaCommand.CANNOT_HAVE_ABILITIES_KEY, "%s cannot have abilities");
            translations.add(HaemaCommand.NO_SUCH_ABILITY_KEY, "No such ability %s");
            translations.add(HaemaCommand.CONVERT_SUCCESS, "%s is now a vampire");
            translations.add(HaemaCommand.CONVERT_FAILURE, "Could not convert %s");
            translations.add(HaemaCommand.DECONVERT_SUCCESS, "%s is no longer a vampire");
            translations.add(HaemaCommand.DECONVERT_FAILURE, "Could not deconvert %s");
            translations.add(HaemaCommand.QUERY_YES, "YES");
            translations.add(HaemaCommand.QUERY_NO, "NO");
            translations.add(HaemaCommand.QUERY_VAMPIRISM_SOURCE, "(Vampirism Source: %1$s)");
            translations.add(HaemaCommand.QUERY_NO_ABILITIES_LINE, "%1$s has no abilities.");
            translations.add(HaemaCommand.QUERY_ABILITIES_LINE, "%1$s has %2$s abilities:");
            translations.add(HaemaCommand.QUERY_FIRST_LINE, "Vampire information for %1$s:");
            translations.add(HaemaCommand.QUERY_IS_VAMPIRE_LINE, "Is %1$s a vampire: %2$s %3$s");
            translations.add(HaemaCommand.QUERY_BLOOD_LINE, "%1$s has %2$s blood");
            translations.add(HaemaCommand.QUERY_ABILITY_LINE, " - %1$s");
            translations.add(HaemaCommand.BLOOD_SET_SUCCESS, "%1$s now has %2$s blood");
            translations.add(HaemaCommand.BLOOD_QUALITY_QUERY_NONE, "%1$s has no blood");
            translations.add(HaemaCommand.BLOOD_QUALITY_QUERY_RESULT, "%1$s has %2$s blood");
            translations.add(HaemaCommand.ABILITY_ADDED, "Given %1$s ability %2$s");
            translations.add(HaemaCommand.ABILITY_REMOVED, "Removed ability %2$s from %1$s");
            translations.add(HaemaCommand.CREATE_MULTIBLOCK_PATTERN_TOO_MANY_BLOCKSTATES, "⚠ Too many blockstates to represent. Extra blockstates are represented as '?'");
            translations.add(HaemaCommand.SPAWN_PATROL_SUCCESS, "Spawned a patrol of %1$s hunters at %2$s");
            translations.add(HaemaCommand.SPAWN_PATROL_FAILURE, "Failed to spawn a patrol at %1$s");
            translations.add(Haema.TAB, "Haema");
            for (var block : HaemaContent.ContentBlocks.BLOOD_BLOCK.values()) {
                translations.add(block, "%1$s Blood");
            }
            for (var cauldron : HaemaContent.ContentBlocks.BLOOD_CAULDRON.values()) {
                translations.add(cauldron, "Blood Cauldron");
            }
            translations.add(HaemaContent.ContentItems.EMPTY_INJECTOR, "Empty Blood Injector");
            for (var item : HaemaContent.ContentItems.INJECTORS.values()) {
                translations.add(item, "Blood Injector");
            }
            translations.add(InjectorItem.DESCRIPTION_TRANSLATION_KEY, "Contains %1$s blood");
            for (var item : HaemaContent.ContentItems.BUCKETS.values()) {
                translations.add(item, "%1$s Blood Bucket");
            }
            for (var item : HaemaContent.ContentItems.BOTTLES.values()) {
                translations.add(item, "Blood Bottle");
            }
            translations.add(BloodBottleItem.DESCRIPTION_TRANSLATION_KEY, "Contains %1$s blood");
            for (var quality : BloodQuality.values()) {
                translations.add(quality.translationKey, quality.getSerializedName().substring(0, 1).toUpperCase(Locale.ROOT) + quality.getSerializedName().substring(1));
            }
            translations.add(HaemaHunters.HunterItems.VAMPIRE_HUNTER_CONTRACT, "Vampire Hunter Contract");
            translations.add(VampireHunterContractItem.FULFILLED_TRANSLATION_KEY, "Fulfilled");
            translations.add(VampireHunterContractItem.UNFULFILLED_TRANSLATION_KEY, "Unfulfilled");
            translations.add(VampireHunterContractItem.NO_TARGET_TRANSLATION_KEY, "Kill a vampire.");
            translations.add(VampireHunterContractItem.TARGET_TRANSLATION_KEY, "Kill the vampire %1$s.");
        }
    }

    public static class HaemaRecipeProvider extends FabricRecipeProvider {
        public HaemaRecipeProvider(FabricDataOutput output) {
            super(output);
        }

        @Override
        public void buildRecipes(Consumer<FinishedRecipe> exporter) {
            BloodFillingRecipe.Builder.create()
                    .empty(Ingredient.of(HaemaContent.ContentItems.EMPTY_INJECTOR))
                    .save(exporter, id("injector_filling"));
            BloodFillingRecipe.Builder.create()
                    .empty(Ingredient.of(Items.GLASS_BOTTLE))
                    .save(exporter, id("bottle_filling"));
        }
    }

    public static class HaemaBarterLootProvider extends SimpleFabricLootTableProvider {

        public HaemaBarterLootProvider(FabricDataOutput output) {
            super(output, LootContextParamSets.PIGLIN_BARTER);
        }

        @Override
        public void accept(BiConsumer<ResourceLocation, LootTable.Builder> out) {
            out.accept(VampireHunter.PAYMENT_LOOT_TABLE, new LootTable.Builder().withPool(
                    new LootPool.Builder().add(
                            createEntry(Items.GOLD_INGOT, 6, 20, 30)).add(
                            createEntry(Items.EMERALD, 30, 40, 40)).add(
                            createEntry(Items.CROSSBOW, 20, Enchantments.QUICK_CHARGE, Enchantments.PIERCING)).add(
                            createEntry(Items.SPECTRAL_ARROW, 9, 20, 20)).add(
                            createEntry(Items.DIAMOND, 5, 10, 5))));
        }

        private static LootPoolEntryContainer.Builder<?> createEntry(ItemLike item, int minCount, int maxCount, int weight) {
            return LootItem.lootTableItem(item).apply(SetItemCountFunction.setCount(UniformGenerator.between(minCount, maxCount))).setWeight(weight);
        }


        private static LootPoolEntryContainer.Builder<?> createEntry(ItemLike item, int weight, Enchantment... enchantments) {
            var enchantmentBuilder = new EnchantRandomlyFunction.Builder();
            for (var enchant : enchantments) {
                enchantmentBuilder = enchantmentBuilder.withEnchantment(enchant);
            }
            return LootItem.lootTableItem(item).apply(enchantmentBuilder).setWeight(weight);
        }
    }
}
