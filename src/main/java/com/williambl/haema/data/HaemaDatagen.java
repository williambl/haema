package com.williambl.haema.data;

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes;
import com.mojang.datafixers.util.Function3;
import com.williambl.dfunc.api.DTypes;
import com.williambl.dfunc.api.functions.BlockInWorldDFunctions;
import com.williambl.dfunc.api.functions.EntityDFunctions;
import com.williambl.dfunc.api.functions.ItemStackDFunctions;
import com.williambl.dfunc.api.functions.LevelDFunctions;
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
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import com.williambl.haema.vampire.ability.powers.vision.VampireVisionVampireAbilityPower;
import com.williambl.haema.vampire_mobs.HaemaVampireMobs;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.lang.VValue;
import com.williambl.vampilang.stdlib.ArithmeticVFunctions;
import com.williambl.vampilang.stdlib.LogicVFunctions;
import com.williambl.vampilang.stdlib.StandardVFunctions;
import com.williambl.vampilang.stdlib.StandardVTypes;
import io.github.apace100.apoli.util.Comparison;
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
import net.minecraft.world.level.storage.loot.LootPool;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.entries.LootItem;
import net.minecraft.world.level.storage.loot.entries.LootPoolEntryContainer;
import net.minecraft.world.level.storage.loot.functions.EnchantRandomlyFunction;
import net.minecraft.world.level.storage.loot.functions.SetItemCountFunction;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.minecraft.world.level.storage.loot.providers.number.UniformGenerator;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
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
            var drinkingAbility = this.createDrinkingAbility(entries);
            defaultAbilties.add(drinkingAbility);

            defaultAbilties.addAll(vampiricStrengthAbilities);


            entries.add(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR, new VampirismSource(Set.of(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR, HaemaVampires.VampirismSources.COMMAND), defaultAbilties, VExpression.value(StandardVTypes.BOOLEAN, false), VExpression.value(StandardVTypes.BOOLEAN, false))); //TODO
            entries.add(HaemaVampires.VampirismSources.COMMAND, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of(), VExpression.value(StandardVTypes.BOOLEAN, true), VExpression.value(StandardVTypes.BOOLEAN, true)));
            entries.add(HaemaVampireMobs.VampireMobVampirismSources.VAMPIRAGER_SPAWN, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of(healingAbility, damageModificationAbility, drinkingAbility), VExpression.value(StandardVTypes.BOOLEAN, true), VExpression.value(StandardVTypes.BOOLEAN, false)));

            entries.add(ResourceKey.create(RitualArae.REGISTRY_KEY, id("basic")), new RitualArae(new MultiblockFilter(
                    new char[][][]{
                            new char[][]{
                                    "ccccc".toCharArray(),
                                    "ccccc".toCharArray(),
                                    "ccccc".toCharArray(),
                                    "ccccc".toCharArray(),
                                    "ccccc".toCharArray()
                            },
                            new char[][]{
                                    " CCC ".toCharArray(),
                                    "C   C".toCharArray(),
                                    "C r C".toCharArray(),
                                    "C   C".toCharArray(),
                                    " CCC ".toCharArray()
                            },
                            new char[][]{
                                    "     ".toCharArray(),
                                    "     ".toCharArray(),
                                    "     ".toCharArray(),
                                    "     ".toCharArray(),
                                    "     ".toCharArray()
                            }
                    },
                    'r',
                    Map.of(
                            'c', VExpression.functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", VExpression.value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.CRIMSON_PLANKS)), "block", VExpression.variable("block"))),
                            'C', VExpression.functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", VExpression.value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.CANDLE)), "block", VExpression.variable("block"))),
                            'r', VExpression.functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", VExpression.value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(HaemaRituals.RitualBlocks.RITUAL_ALTAR)), "block", VExpression.variable("block")))),
                    VExpression.value(StandardVTypes.BOOLEAN, true)
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
                            new char[][]{
                                    " ccccc ".toCharArray(),
                                    "c_____c".toCharArray(),
                                    "c_ccc_c".toCharArray(),
                                    "c_crc_c".toCharArray(),
                                    "c_ccc_c".toCharArray(),
                                    "c_____c".toCharArray(),
                                    " ccccc ".toCharArray()
                            },
                            new char[][]{
                                    "C     C".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "C     C".toCharArray()
                            },
                            new char[][]{
                                    "C     C".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "C     C".toCharArray()
                            },
                            new char[][]{
                                    "C     C".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "       ".toCharArray(),
                                    "C     C".toCharArray()
                            },
                            new char[][]{
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
                            'c', VExpression.functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", VExpression.value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.CRIMSON_NYLIUM)), "block", VExpression.variable("block"))),
                            'C', VExpression.functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", VExpression.value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.CRIMSON_STEM)), "block", VExpression.variable("block"))),
                            //TODO this doesn't work due to a null-handling bug in jsonops i think? hmmm
                            //'C', BlockInWorldDFunctions.ADVANCEMENT_PREDICATE.factory().apply(net.minecraft.advancements.critereon.BlockPredicate.Builder.block().of(Blocks.CRIMSON_STEM).setProperties(StatePropertiesPredicate.Builder.properties().hasProperty(BlockStateProperties.AXIS, Direction.Axis.Y).build()).build(), ContextArg.BLOCK.arg()),
                            'r', VExpression.functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate",VExpression.value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(HaemaRituals.RitualBlocks.RITUAL_ALTAR)), "block", VExpression.variable("block"))),
                            'F', VExpression.functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate",VExpression.value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.SOUL_CAMPFIRE)), "block", VExpression.variable("block"))),
                            //'F', BlockInWorldDFunctions.ADVANCEMENT_PREDICATE.factory().apply(net.minecraft.advancements.critereon.BlockPredicate.Builder.block().of(Blocks.SOUL_CAMPFIRE).setProperties(StatePropertiesPredicate.Builder.properties().hasProperty(BlockStateProperties.LIT, true).build()).build(), ContextArg.BLOCK.arg()),
                            '_', VExpression.value(StandardVTypes.BOOLEAN, true)),
                    VExpression.value(StandardVTypes.BOOLEAN, true)),
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
                    .trigger(new RightClickRitualTrigger(VExpression.value(StandardVTypes.BOOLEAN, true)))
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
            var healingAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(
                    new HealingVampireAbilityPower(VExpression.functionApplication(LogicVFunctions.AND, Map.of("operands", VExpression.list(List.of(
                            VExpression.functionApplication(LevelDFunctions.BOOLEAN_GAME_RULE, Map.of("rule", VExpression.value(StandardVTypes.STRING, "naturalRegeneration"), "level", VExpression.variable("level"))),
                            VExpression.functionApplication(LogicVFunctions.NOT, Map.of("operand", VExpression.functionApplication(EntityDFunctions.DEAD_OR_DYING, Map.of("entity", VExpression.variable("entity"))))),
                            VExpression.functionApplication(StandardVFunctions.EQUALS, Map.of("a", (VExpression.functionApplication(ArithmeticVFunctions.MODULO, Map.of("a", (VExpression.functionApplication(EntityDFunctions.AGE, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 20.0))))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 0.)))),
                            VExpression.functionApplication(LogicVFunctions.NOT, Map.of("operand", VExpression.functionApplication(EntityDFunctions.HAS_EFFECT, Map.of("effect", VExpression.value(DTypes.MOB_EFFECT, HaemaVampires.VampireMobEffects.SUNLIGHT_SICKNESS), "entity", VExpression.variable("entity"))))),
                            VExpression.functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (VExpression.functionApplication(EntityDFunctions.HEALTH, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.functionApplication(EntityDFunctions.ATTRIBUTE, Map.of("attribute", VExpression.value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", VExpression.variable("entity")))))),
                            VExpression.functionApplication(LogicVFunctions.OR, Map.of("operands", VExpression.list(List.of(
                                    VExpression.functionApplication(LogicVFunctions.AND, Map.of("operands", VExpression.list(List.of(
                                            VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 19.0)))),
                                            VExpression.functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (VExpression.functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (VExpression.functionApplication(EntityDFunctions.HEALTH, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.functionApplication(EntityDFunctions.ATTRIBUTE_BASE, Map.of("attribute", VExpression.value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", VExpression.variable("entity"))))))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 20.0))))
                                    )))),
                                    VExpression.functionApplication(LogicVFunctions.AND, Map.of("operands", VExpression.list(List.of(
                                            VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 14.0)))),
                                            VExpression.functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (VExpression.functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (VExpression.functionApplication(EntityDFunctions.HEALTH, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.functionApplication(EntityDFunctions.ATTRIBUTE_BASE, Map.of("attribute", VExpression.value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", VExpression.variable("entity"))))))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 10.0))))
                                    )))),
                                    VExpression.functionApplication(LogicVFunctions.AND, Map.of("operands", VExpression.list(List.of(
                                            VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 10.0)))),
                                            VExpression.functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (VExpression.functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (VExpression.functionApplication(EntityDFunctions.HEALTH, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.functionApplication(EntityDFunctions.ATTRIBUTE_BASE, Map.of("attribute", VExpression.value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", VExpression.variable("entity"))))))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 6.0))))
                                    )))),
                                    VExpression.functionApplication(LogicVFunctions.AND, Map.of("operands", VExpression.list(List.of(
                                            VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 8.0)))),
                                            VExpression.functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (VExpression.functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (VExpression.functionApplication(EntityDFunctions.HEALTH, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.functionApplication(EntityDFunctions.ATTRIBUTE_BASE, Map.of("attribute", VExpression.value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", VExpression.variable("entity"))))))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 0.0))))
                                    )))))))))))),
                            VExpression.value(StandardVTypes.NUMBER, (double) 1.0),
                            VExpression.functionApplication(ArithmeticVFunctions.POLYNOMIAL, Map.of("coefficients", VExpression.list(List.of(VExpression.value(StandardVTypes.NUMBER, 1.05), VExpression.value(StandardVTypes.NUMBER, 0.0), VExpression.value(StandardVTypes.NUMBER, -1.0))), "input", (VExpression.functionApplication(ArithmeticVFunctions.DIVIDE, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.functionApplication(HaemaDFunctions.MAX_BLOOD, Map.of("entity", VExpression.variable("entity"))))))))))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("healing"));
            entries.add(key, healingAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createReachAbility(Entries entries) {
            var reachAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new AttributeVampireAbilityPower(Set.of(
                    new AttributeVampireAbilityPower.Data(
                            ReachEntityAttributes.REACH,
                            new AttributeModifier(UUID.fromString("0eb4fc5f-71d5-4440-b517-bcc18e1df6f4"), "Vampire Reach bonus", 2.0, AttributeModifier.Operation.ADDITION),
                            VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 6.0))))
                    ),
                    new AttributeVampireAbilityPower.Data(
                            ReachEntityAttributes.ATTACK_RANGE,
                            new AttributeModifier(UUID.fromString("3267a46b-2b48-429f-a3a8-439aa87a876d"), "Vampire Attack Range bonus", 2.0, AttributeModifier.Operation.ADDITION),
                            VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 6.0))))
                    )
            ))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("reach"));
            entries.add(key, reachAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createHealthBoostAbility(Entries entries) {
            var healthBoostAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new AttributeVampireAbilityPower(Set.of(
                    new AttributeVampireAbilityPower.Data(
                            Attributes.MAX_HEALTH,
                            new AttributeModifier(UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92"), "Vampire Max Health bonus", 1.0, AttributeModifier.Operation.MULTIPLY_BASE),
                            VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 6.0))))
                    )
            ))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("health_boost"));
            entries.add(key, healthBoostAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createSunlightSicknessAbility(Entries entries) {
            var sunlightSicknessAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new EffectVampireAbilityPower(Set.of(
                    new EffectVampireAbilityPower.Data(
                            HaemaVampires.VampireMobEffects.SUNLIGHT_SICKNESS,
                            VExpression.value(StandardVTypes.NUMBER, (double) 0.0),
                            VExpression.value(StandardVTypes.NUMBER, (double) 10.0),
                            VExpression.value(StandardVTypes.BOOLEAN, false),
                            VExpression.value(StandardVTypes.BOOLEAN, true),
                            VExpression.value(StandardVTypes.BOOLEAN, true),
                            VExpression.functionApplication(LogicVFunctions.AND, Map.of("operands", VExpression.list(List.of(
                                    VExpression.functionApplication(LevelDFunctions.BOOLEAN_GAME_RULE, Map.of("rule", VExpression.value(StandardVTypes.STRING, HaemaVampires.VampireGameRules.VAMPIRES_BURN.getId()), "level", VExpression.variable("level"))),
                                    VExpression.functionApplication(EntityDFunctions.IS_SURVIVAL_LIKE, Map.of("entity", VExpression.variable("entity"))),
                                    VExpression.functionApplication(LogicVFunctions.OR, Map.of("operands", VExpression.list(List.of(
                                            VExpression.functionApplication(LogicVFunctions.AND, Map.of("operands", VExpression.list(List.of(
                                                    VExpression.functionApplication(EntityDFunctions.CAN_SEE_SKY, Map.of("entity", VExpression.variable("entity"))),
                                                    VExpression.functionApplication(LevelDFunctions.IS_DAY, Map.of("level", VExpression.variable("level"))),
                                                    VExpression.functionApplication(LogicVFunctions.NOT, Map.of("operand", VExpression.functionApplication(LevelDFunctions.IS_RAINING, Map.of("level", VExpression.variable("level"))))))))),
                                            VExpression.functionApplication(HaemaDFunctions.TRIGGER_BURN_EVENT, Map.of("entity", VExpression.variable("entity"))))))),
                                    VExpression.functionApplication(LogicVFunctions.NOT, Map.of("operand", VExpression.functionApplication(HaemaDFunctions.PREVENT_BURN_EVENT, Map.of("entity", VExpression.variable("entity"))))))))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("sunlight_sickness"));
            entries.add(key, sunlightSicknessAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createVampiricWeaknessAbility(Entries entries) {
            var vampiricWeaknessAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new EffectVampireAbilityPower(Set.of(
                    new EffectVampireAbilityPower.Data(
                            HaemaVampires.VampireMobEffects.VAMPIRIC_WEAKNESS,
                            VExpression.functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (VExpression.value(StandardVTypes.NUMBER, (double) 3.0)), "b", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))))),
                            VExpression.value(StandardVTypes.NUMBER, (double) 5.0),
                            VExpression.value(StandardVTypes.BOOLEAN, false),
                            VExpression.value(StandardVTypes.BOOLEAN, true),
                            VExpression.value(StandardVTypes.BOOLEAN, true),
                            VExpression.functionApplication(StandardVFunctions.LESS_THAN_OR_EQUAL, Map.of("a", (VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 3.0)))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("vampiric_weakness"));
            entries.add(key, vampiricWeaknessAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createDamageModificationAbility(Entries entries) {
            var damageModificationAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new DamageModificationAbilityPower(
                    VExpression.functionApplication(ArithmeticVFunctions.MULTIPLY, Map.of(
                            "a", VExpression.variable("damage_amount"),
                            "b", VExpression.functionApplication(ArithmeticVFunctions.MAX, Map.of("a", (
                                    VExpression.functionApplication(StandardVFunctions.IF_ELSE, Map.of(
                                            "a", (VExpression.value(StandardVTypes.NUMBER, (double) 1.25)),
                                            "b", (VExpression.value(StandardVTypes.NUMBER, (double) 1.0)),
                                            "predicate", VExpression.functionApplication(LogicVFunctions.OR, Map.of("operands", VExpression.list(List.of(
                                                    VExpression.functionApplication(ItemStackDFunctions.TAG, Map.of("tag", VExpression.value(DTypes.TAG.with(0, DTypes.DAMAGE_SOURCE), HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_DAMAGE), "input", VExpression.variable("damage_source"))),
                                                    VExpression.functionApplication(ItemStackDFunctions.TAG, Map.of("tag", VExpression.value(DTypes.TAG.with(0, DTypes.ITEM_STACK), HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_WEAPONS), "input", VExpression.variable("weapon")))))))))),
                            "b", VExpression.functionApplication(ItemStackDFunctions.ENCHANTMENT_LEVEL, Map.of("enchantment", VExpression.value(DTypes.ENCHANTMENT, Enchantments.SMITE), "item", VExpression.variable("weapon"))))))),
                    VExpression.functionApplication(LogicVFunctions.OR, Map.of("operands", VExpression.list(List.of(VExpression.functionApplication(StandardVFunctions.GREATER_THAN, Map.of("a", (VExpression.functionApplication(ItemStackDFunctions.ENCHANTMENT_LEVEL, Map.of("enchantment", VExpression.value(DTypes.ENCHANTMENT, Enchantments.SMITE), "item", VExpression.variable("weapon")))), "b", (VExpression.value(StandardVTypes.NUMBER, (double) 0.0)))),
                            VExpression.functionApplication(ItemStackDFunctions.TAG, Map.of("tag", VExpression.value(DTypes.TAG.with(0, DTypes.DAMAGE_SOURCE), HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_DAMAGE), "input", VExpression.variable("damage_source"))),
                            VExpression.functionApplication(ItemStackDFunctions.TAG, Map.of("tag", VExpression.value(DTypes.TAG.with(0, DTypes.ITEM_STACK), HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_WEAPONS), "input", VExpression.variable("weapon"))))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("damage_modification"));
            entries.add(key, damageModificationAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createVampireVisionAbility(Entries entries) {
            var vampireVisionAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(
                    new VampireVisionVampireAbilityPower()));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("vampire_vision"));
            entries.add(key, vampireVisionAbility);
            return key;
        }


        private ResourceKey<VampireAbility> createStrengthAbility(Entries entries, int level, Collection<ResourceKey<VampireAbility>> before) {
            //TODO fix match so we can use that
            Function3<Double, Double, VExpression, VExpression> ifElseCreator = (blood, amplifier, elze) ->
                    VExpression.functionApplication(StandardVFunctions.IF_ELSE, Map.of(
                            "predicate",
                            VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of(
                                            "a", VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity"))),
                                            "b", VExpression.value(StandardVTypes.NUMBER, (double) blood))),
                            "a", VExpression.value(StandardVTypes.NUMBER, (double) amplifier),
                            "b", elze));
            var a = VExpression.value(StandardVTypes.NUMBER, 0.0);
            var b = ifElseCreator.apply(14.0, 1.0, a);
            var c = ifElseCreator.apply(19.0, 2.0, b);
            var strengthAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, true), Set.copyOf(before), Set.of(), Set.copyOf(before), List.of(
                    new EffectVampireAbilityPower(Set.of(
                            new EffectVampireAbilityPower.Data(
                                    HaemaVampires.VampireMobEffects.VAMPIRIC_STRENGTH,
                                    c,
                                    VExpression.value(StandardVTypes.NUMBER, (double) 40.0),
                                    VExpression.value(StandardVTypes.BOOLEAN, false),
                                    VExpression.value(StandardVTypes.BOOLEAN, true),
                                    VExpression.value(StandardVTypes.BOOLEAN, true),
                                    VExpression.functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", VExpression.functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", VExpression.variable("entity"))), "b", VExpression.value(StandardVTypes.NUMBER, (double) 10.0))))))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("strength/"+level));
            entries.add(key, strengthAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createDrinkingAbility(Entries entries) {
            var drinkingAbility = new VampireAbility(true, VExpression.value(StandardVTypes.BOOLEAN, true), Set.of(), Set.of(), Set.of(), List.of(
                    new DrinkingAbilityPower(
                            VExpression.value(StandardVTypes.NUMBER, (double) 10.0),
                            VExpression.value(StandardVTypes.NUMBER, (double) (double) BloodApi.bloodUnitsToDroplets(1)),
                            VExpression.value(StandardVTypes.BOOLEAN, true),
                            List.of())));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("drinking"));
            entries.add(key, drinkingAbility);
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
            translations.add(HaemaCommand.CREATE_CONTRACT_SUCCESS, "Created a contract with a target of %1$s");
            translations.add(HaemaCommand.CREATE_CONTRACT_NO_TARGET, "⚠ Created a contract, but no targets wera available");
            translations.add(Haema.TAB.key(), "Haema");
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
        public void generate(BiConsumer<ResourceLocation, LootTable.Builder> out) {
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
