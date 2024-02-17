package com.williambl.haema.data;

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes;
import com.mojang.datafixers.util.Function3;
import com.williambl.actions.Actions;
import com.williambl.dfunc.api.DTypes;
import com.williambl.dfunc.api.functions.BlockInWorldDFunctions;
import com.williambl.dfunc.api.functions.EntityDFunctions;
import com.williambl.dfunc.api.functions.ItemStackDFunctions;
import com.williambl.dfunc.api.functions.LevelDFunctions;
import com.williambl.haema.Haema;
import com.williambl.haema.HaemaCommand;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.IconProvider;
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
import com.williambl.haema.vampire.ability.powers.dash.DashAbilityPower;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import com.williambl.haema.vampire.ability.powers.hungerbar.ModifyHungerBarAbilityPower;
import com.williambl.haema.vampire.ability.powers.sleep.SleepInDayAbilityPower;
import com.williambl.haema.vampire.ability.powers.vision.VampireVisionVampireAbilityPower;
import com.williambl.haema.vampire_mobs.HaemaVampireMobs;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.ArithmeticVFunctions;
import com.williambl.vampilang.stdlib.LogicVFunctions;
import com.williambl.vampilang.stdlib.StandardVFunctions;
import com.williambl.vampilang.stdlib.StandardVTypes;
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
import net.minecraft.data.recipes.RecipeOutput;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvents;
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

import static com.williambl.haema.Haema.id;
import static com.williambl.vampilang.lang.VExpression.*;

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
                    .add(EntityType.PLAYER)
                    .add(EntityType.VILLAGER);
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
            var drinkingAbility = this.createDrinkingAbility(entries);
            defaultAbilties.add(drinkingAbility);
            var modifyHungerBarAbility = this.createModifyHungerBarAbility(entries);
            defaultAbilties.add(modifyHungerBarAbility);
            var dashAbilities = new ArrayList<ResourceKey<VampireAbility>>();
            for (int i = 0; i < 3; i++) {
                dashAbilities.add(this.createDashAbility(entries, i+1, dashAbilities));
            }
            defaultAbilties.addAll(dashAbilities);
            var mistFormAbility = this.createMistFormAbility(entries);
            defaultAbilties.add(mistFormAbility);
            var invisibilityAbilities = new ArrayList<ResourceKey<VampireAbility>>();
            for (int i = 0; i < 3; i++) {
                invisibilityAbilities.add(this.createInvisibilityAbility(entries, i+1, invisibilityAbilities));
            }
            defaultAbilties.addAll(invisibilityAbilities);
            var sleepInDayAbility = this.createSleepInDayAbility(entries);
            defaultAbilties.add(sleepInDayAbility);


            entries.add(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR, new VampirismSource(Set.of(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR, HaemaVampires.VampirismSources.COMMAND), defaultAbilties, value(StandardVTypes.BOOLEAN, false), value(StandardVTypes.BOOLEAN, false))); //TODO
            entries.add(HaemaVampires.VampirismSources.COMMAND, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of(), value(StandardVTypes.BOOLEAN, true), value(StandardVTypes.BOOLEAN, true)));
            entries.add(HaemaVampireMobs.VampireMobVampirismSources.VAMPIRAGER_SPAWN, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of(healingAbility, damageModificationAbility, drinkingAbility, sunlightSicknessAbility, dashAbilities.get(0), dashAbilities.get(1), dashAbilities.get(2)), value(StandardVTypes.BOOLEAN, true), value(StandardVTypes.BOOLEAN, false)));

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
                            'c', functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.CRIMSON_PLANKS)), "block", variable("block"))),
                            'C', functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.CANDLE)), "block", variable("block"))),
                            'r', functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(HaemaRituals.RitualBlocks.RITUAL_ALTAR)), "block", variable("block")))),
                    value(StandardVTypes.BOOLEAN, true)
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
                            'c', functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.CRIMSON_NYLIUM)), "block", variable("block"))),
                            'C', functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.CRIMSON_STEM)), "block", variable("block"))),
                            //TODO this doesn't work due to a null-handling bug in jsonops i think? hmmm
                            //'C', BlockInWorldDFunctions.ADVANCEMENT_PREDICATE.factory().apply(net.minecraft.advancements.critereon.BlockPredicate.Builder.block().of(Blocks.CRIMSON_STEM).setProperties(StatePropertiesPredicate.Builder.properties().hasProperty(BlockStateProperties.AXIS, Direction.Axis.Y).build()).build(), ContextArg.BLOCK.arg()),
                            'r', functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(HaemaRituals.RitualBlocks.RITUAL_ALTAR)), "block", variable("block"))),
                            'F', functionApplication(BlockInWorldDFunctions.WORLDGEN_PREDICATE, Map.of("predicate", value(DTypes.BLOCK_WORLDGEN_PREDICATE, BlockPredicate.matchesBlocks(Blocks.SOUL_CAMPFIRE)), "block", variable("block"))),
                            //'F', BlockInWorldDFunctions.ADVANCEMENT_PREDICATE.factory().apply(net.minecraft.advancements.critereon.BlockPredicate.Builder.block().of(Blocks.SOUL_CAMPFIRE).setProperties(StatePropertiesPredicate.Builder.properties().hasProperty(BlockStateProperties.LIT, true).build()).build(), ContextArg.BLOCK.arg()),
                            '_', value(StandardVTypes.BOOLEAN, true)),
                    value(StandardVTypes.BOOLEAN, true)),
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
                    .trigger(new RightClickRitualTrigger(value(StandardVTypes.BOOLEAN, true)))
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
            var healingAbility = new VampireAbility(true, IconProvider.of(id("textures/gui/sprites/hud/blood_full.png"), 9), false, value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(
                    new HealingVampireAbilityPower(functionApplication(LogicVFunctions.AND, Map.of("operands", list(List.of(
                            functionApplication(LevelDFunctions.BOOLEAN_GAME_RULE, Map.of("rule", value(StandardVTypes.STRING, "naturalRegeneration"), "level", variable("level"))),
                            functionApplication(LogicVFunctions.NOT, Map.of("operand", functionApplication(EntityDFunctions.DEAD_OR_DYING, Map.of("entity", variable("entity"))))),
                            functionApplication(StandardVFunctions.EQUALS, Map.of("a", (functionApplication(ArithmeticVFunctions.MODULO, Map.of("a", (functionApplication(EntityDFunctions.AGE, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 20.0))))), "b", (value(StandardVTypes.NUMBER, 0.)))),
                            functionApplication(LogicVFunctions.NOT, Map.of("operand", functionApplication(EntityDFunctions.HAS_EFFECT, Map.of("effect", value(DTypes.MOB_EFFECT, HaemaVampires.VampireMobEffects.SUNLIGHT_SICKNESS), "entity", variable("entity"))))),
                            functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (functionApplication(EntityDFunctions.HEALTH, Map.of("entity", variable("entity")))), "b", (functionApplication(EntityDFunctions.ATTRIBUTE, Map.of("attribute", value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", variable("entity")))))),
                            functionApplication(LogicVFunctions.OR, Map.of("operands", list(List.of(
                                    functionApplication(LogicVFunctions.AND, Map.of("operands", list(List.of(
                                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 19.0)))),
                                            functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (functionApplication(EntityDFunctions.HEALTH, Map.of("entity", variable("entity")))), "b", (functionApplication(EntityDFunctions.ATTRIBUTE_BASE, Map.of("attribute", value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", variable("entity"))))))), "b", (value(StandardVTypes.NUMBER, 20.0))))
                                    )))),
                                    functionApplication(LogicVFunctions.AND, Map.of("operands", list(List.of(
                                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 14.0)))),
                                            functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (functionApplication(EntityDFunctions.HEALTH, Map.of("entity", variable("entity")))), "b", (functionApplication(EntityDFunctions.ATTRIBUTE_BASE, Map.of("attribute", value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", variable("entity"))))))), "b", (value(StandardVTypes.NUMBER, 10.0))))
                                    )))),
                                    functionApplication(LogicVFunctions.AND, Map.of("operands", list(List.of(
                                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 10.0)))),
                                            functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (functionApplication(EntityDFunctions.HEALTH, Map.of("entity", variable("entity")))), "b", (functionApplication(EntityDFunctions.ATTRIBUTE_BASE, Map.of("attribute", value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", variable("entity"))))))), "b", (value(StandardVTypes.NUMBER, 6.0))))
                                    )))),
                                    functionApplication(LogicVFunctions.AND, Map.of("operands", list(List.of(
                                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 8.0)))),
                                            functionApplication(StandardVFunctions.LESS_THAN, Map.of("a", (functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (functionApplication(EntityDFunctions.HEALTH, Map.of("entity", variable("entity")))), "b", (functionApplication(EntityDFunctions.ATTRIBUTE_BASE, Map.of("attribute", value(DTypes.ATTRIBUTE, Attributes.MAX_HEALTH), "entity", variable("entity"))))))), "b", (value(StandardVTypes.NUMBER, 0.0))))
                                    )))))))))))),
                            value(StandardVTypes.NUMBER, 1.0),
                            functionApplication(ArithmeticVFunctions.POLYNOMIAL, Map.of("coefficients", list(List.of(value(StandardVTypes.NUMBER, 1.05), value(StandardVTypes.NUMBER, 0.0), value(StandardVTypes.NUMBER, -1.0))), "input", (functionApplication(ArithmeticVFunctions.DIVIDE, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (functionApplication(HaemaDFunctions.MAX_BLOOD, Map.of("entity", variable("entity"))))))))))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("healing"));
            entries.add(key, healingAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createReachAbility(Entries entries) {
            var reachAbility = new VampireAbility(true, IconProvider.of(Items.FEATHER), false, value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new AttributeVampireAbilityPower(Set.of(
                    new AttributeVampireAbilityPower.Data(
                            ReachEntityAttributes.REACH,
                            new AttributeModifier(UUID.fromString("0eb4fc5f-71d5-4440-b517-bcc18e1df6f4"), "Vampire Reach bonus", 2.0, AttributeModifier.Operation.ADDITION),
                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 6.0))))
                    ),
                    new AttributeVampireAbilityPower.Data(
                            ReachEntityAttributes.ATTACK_RANGE,
                            new AttributeModifier(UUID.fromString("3267a46b-2b48-429f-a3a8-439aa87a876d"), "Vampire Attack Range bonus", 2.0, AttributeModifier.Operation.ADDITION),
                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 6.0))))
                    )
            ))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("reach"));
            entries.add(key, reachAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createHealthBoostAbility(Entries entries) {
            var healthBoostAbility = new VampireAbility(true, IconProvider.of(new ResourceLocation("textures/mob_effect/health_boost.png"), 11), false, value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new AttributeVampireAbilityPower(Set.of(
                    new AttributeVampireAbilityPower.Data(
                            Attributes.MAX_HEALTH,
                            new AttributeModifier(UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92"), "Vampire Max Health bonus", 1.0, AttributeModifier.Operation.MULTIPLY_BASE),
                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 6.0))))
                    )
            ))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("health_boost"));
            entries.add(key, healthBoostAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createSunlightSicknessAbility(Entries entries) {
            var sunlightSicknessAbility = new VampireAbility(true, IconProvider.of(id("textures/mob_effect/sunlight_sickness.png"), 11), false, value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new EffectVampireAbilityPower(Set.of(
                    new EffectVampireAbilityPower.Data(
                            HaemaVampires.VampireMobEffects.SUNLIGHT_SICKNESS,
                            value(StandardVTypes.NUMBER, 0.0),
                            value(StandardVTypes.NUMBER, 10.0),
                            value(StandardVTypes.BOOLEAN, false),
                            value(StandardVTypes.BOOLEAN, true),
                            value(StandardVTypes.BOOLEAN, true),
                            functionApplication(LogicVFunctions.AND, Map.of("operands", list(List.of(
                                    functionApplication(LevelDFunctions.BOOLEAN_GAME_RULE, Map.of("rule", value(StandardVTypes.STRING, HaemaVampires.VampireGameRules.VAMPIRES_BURN.getId()), "level", variable("level"))),
                                    functionApplication(EntityDFunctions.IS_SURVIVAL_LIKE, Map.of("entity", variable("entity"))),
                                    functionApplication(LogicVFunctions.OR, Map.of("operands", list(List.of(
                                            functionApplication(LogicVFunctions.AND, Map.of("operands", list(List.of(
                                                    functionApplication(EntityDFunctions.CAN_SEE_SKY, Map.of("entity", variable("entity"))),
                                                    functionApplication(LevelDFunctions.IS_DAY, Map.of("level", variable("level"))),
                                                    functionApplication(LogicVFunctions.NOT, Map.of("operand", functionApplication(LevelDFunctions.IS_RAINING, Map.of("level", variable("level"))))))))),
                                            functionApplication(HaemaDFunctions.TRIGGER_BURN_EVENT, Map.of("entity", variable("entity"))))))),
                                    functionApplication(LogicVFunctions.NOT, Map.of("operand", functionApplication(HaemaDFunctions.PREVENT_BURN_EVENT, Map.of("entity", variable("entity"))))))))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("sunlight_sickness"));
            entries.add(key, sunlightSicknessAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createVampiricWeaknessAbility(Entries entries) {
            var vampiricWeaknessAbility = new VampireAbility(true, IconProvider.of(id("textures/mob_effect/vampiric_weakness.png"), 11), false, value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new EffectVampireAbilityPower(Set.of(
                    new EffectVampireAbilityPower.Data(
                            HaemaVampires.VampireMobEffects.VAMPIRIC_WEAKNESS,
                            functionApplication(ArithmeticVFunctions.SUBTRACT, Map.of("a", (value(StandardVTypes.NUMBER, 3.0)), "b", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))))),
                            value(StandardVTypes.NUMBER, 5.0),
                            value(StandardVTypes.BOOLEAN, false),
                            value(StandardVTypes.BOOLEAN, true),
                            value(StandardVTypes.BOOLEAN, true),
                            functionApplication(StandardVFunctions.LESS_THAN_OR_EQUAL, Map.of("a", (functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity")))), "b", (value(StandardVTypes.NUMBER, 3.0)))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("vampiric_weakness"));
            entries.add(key, vampiricWeaknessAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createDamageModificationAbility(Entries entries) {
            var damageModificationAbility = new VampireAbility(true, IconProvider.of(Items.WOODEN_SWORD), false, value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(new DamageModificationAbilityPower(
                    functionApplication(ArithmeticVFunctions.MULTIPLY, Map.of(
                            "a", variable("damage_amount"),
                            "b", functionApplication(ArithmeticVFunctions.MAX, Map.of("a", (
                                    functionApplication(StandardVFunctions.IF_ELSE, Map.of(
                                            "a", (value(StandardVTypes.NUMBER, 1.25)),
                                            "b", (value(StandardVTypes.NUMBER, 1.0)),
                                            "predicate", functionApplication(LogicVFunctions.OR, Map.of("operands", list(List.of(
                                                    functionApplication(ItemStackDFunctions.TAG, Map.of("tag", value(DTypes.TAG.with(0, DTypes.DAMAGE_SOURCE), HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_DAMAGE), "input", variable("damage_source"))),
                                                    functionApplication(ItemStackDFunctions.TAG, Map.of("tag", value(DTypes.TAG.with(0, DTypes.ITEM_STACK), HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_WEAPONS), "input", variable("weapon")))))))))),
                            "b", functionApplication(ItemStackDFunctions.ENCHANTMENT_LEVEL, Map.of("enchantment", value(DTypes.ENCHANTMENT, Enchantments.SMITE), "item", variable("weapon"))))))),
                    functionApplication(LogicVFunctions.OR, Map.of("operands", list(List.of(functionApplication(StandardVFunctions.GREATER_THAN, Map.of("a", (functionApplication(ItemStackDFunctions.ENCHANTMENT_LEVEL, Map.of("enchantment", value(DTypes.ENCHANTMENT, Enchantments.SMITE), "item", variable("weapon")))), "b", (value(StandardVTypes.NUMBER, 0.0)))),
                            functionApplication(ItemStackDFunctions.TAG, Map.of("tag", value(DTypes.TAG.with(0, DTypes.DAMAGE_SOURCE), HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_DAMAGE), "input", variable("damage_source"))),
                            functionApplication(ItemStackDFunctions.TAG, Map.of("tag", value(DTypes.TAG.with(0, DTypes.ITEM_STACK), HaemaVampires.VampireTags.VAMPIRE_EFFECTIVE_WEAPONS), "input", variable("weapon"))))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("damage_modification"));
            entries.add(key, damageModificationAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createVampireVisionAbility(Entries entries) {
            var vampireVisionAbility = new VampireAbility(true, IconProvider.of(new ResourceLocation("textures/mob_effect/night_vision.png"), 11), false, value(StandardVTypes.BOOLEAN, false), Set.of(), Set.of(), Set.of(), List.of(
                    new VampireVisionVampireAbilityPower()));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("vampire_vision"));
            entries.add(key, vampireVisionAbility);
            return key;
        }


        private ResourceKey<VampireAbility> createStrengthAbility(Entries entries, int level, Collection<ResourceKey<VampireAbility>> before) {
            //TODO fix match so we can use that
            Function3<Double, Double, VExpression, VExpression> ifElseCreator = (blood, amplifier, elze) ->
                    functionApplication(StandardVFunctions.IF_ELSE, Map.of(
                            "predicate",
                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of(
                                            "a", functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity"))),
                                            "b", value(StandardVTypes.NUMBER, blood))),
                            "a", value(StandardVTypes.NUMBER, amplifier),
                            "b", elze));
            var a = value(StandardVTypes.NUMBER, 0.0);
            var b = ifElseCreator.apply(14.0, 1.0, a);
            var c = ifElseCreator.apply(19.0, 2.0, b);
            var strengthAbility = new VampireAbility(true, IconProvider.of(Items.IRON_SWORD), false, value(StandardVTypes.BOOLEAN, true), Set.copyOf(before), Set.of(), Set.copyOf(before), List.of(
                    new EffectVampireAbilityPower(Set.of(
                            new EffectVampireAbilityPower.Data(
                                    HaemaVampires.VampireMobEffects.VAMPIRIC_STRENGTH,
                                    c,
                                    value(StandardVTypes.NUMBER, 40.0),
                                    value(StandardVTypes.BOOLEAN, false),
                                    value(StandardVTypes.BOOLEAN, true),
                                    value(StandardVTypes.BOOLEAN, true),
                                    functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of("a", functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity"))), "b", value(StandardVTypes.NUMBER, 10.0))))))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("strength/"+level));
            entries.add(key, strengthAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createDrinkingAbility(Entries entries) {
            var cooldownId = id("drinking");
            var drinkingAbility = new VampireAbility(true, IconProvider.of(Items.POTION), false, value(StandardVTypes.BOOLEAN, true), Set.of(), Set.of(), Set.of(), List.of(
                    new DrinkingAbilityPower(
                            value(StandardVTypes.NUMBER, (double) BloodApi.bloodUnitsToDroplets(1)),
                            functionApplication(LogicVFunctions.AND, Map.of(
                                    "operands", list(List.of(
                                            functionApplication(StandardVFunctions.LESS_THAN_OR_EQUAL, Map.of(
                                                    "a", functionApplication(Actions.GET_COOLDOWN, Map.of("entity", variable("entity"), "cooldown_id", value(DTypes.RESOURCE_LOCATION, cooldownId))),
                                                    "b", value(StandardVTypes.NUMBER, 0.0))),
                                            functionApplication(ItemStackDFunctions.ITEM_IS_EMPTY, Map.of(
                                                    "item", functionApplication(EntityDFunctions.MAIN_HAND_ITEM, Map.of("entity", variable("entity"))))))))),
                            list(List.of(
                                    object(Actions.name(Actions.PLAY_SOUND), Map.of(
                                            "level", variable("level"),
                                            "sound_event", value(DTypes.SOUND_EVENT, SoundEvents.GENERIC_DRINK),
                                            "position", functionApplication(EntityDFunctions.POSITION, Map.of("entity", variable("entity"))),
                                            "pitch", value(StandardVTypes.NUMBER, 1.0),
                                            "volume", value(StandardVTypes.NUMBER, 1.0)
                                    )),
                                object(Actions.name(Actions.SET_COOLDOWN), Map.of(
                                        "entity", variable("entity"),
                                        "cooldown_id", value(DTypes.RESOURCE_LOCATION, cooldownId),
                                        "length", value(StandardVTypes.NUMBER, 10.0))))),
                            List.of(
                                    "key.sneak",
                                    "key.use"))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("drinking"));
            entries.add(key, drinkingAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createModifyHungerBarAbility(Entries entries) {
            var drinkingAbility = new VampireAbility(true, IconProvider.of(Items.COOKED_BEEF), false, value(StandardVTypes.BOOLEAN, true), Set.of(), Set.of(), Set.of(), List.of(
                    new ModifyHungerBarAbilityPower(
                            id("hud/blood_full"),
                            id("hud/blood_half"),
                            id("hud/blood_empty"),
                            id("hud/blood_full"),
                            id("hud/blood_half"),
                            id("hud/blood_empty"),
                            functionApplication(ArithmeticVFunctions.MULTIPLY, Map.of(
                                    "a", functionApplication(ArithmeticVFunctions.DIVIDE, Map.of(
                                            "a", functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity"))),
                                            "b", functionApplication(HaemaDFunctions.MAX_BLOOD, Map.of("entity", variable("entity")))
                                    )),
                                    "b", value(StandardVTypes.NUMBER, 20.0)
                            )))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("modify_hunger_bar"));
            entries.add(key, drinkingAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createDashAbility(Entries entries, int level, Collection<ResourceKey<VampireAbility>> before) {
            int cooldown = 10 * level;
            var cooldownId = id("dash");
            var dashAbility = new VampireAbility(true, IconProvider.of(Items.FEATHER), true, value(StandardVTypes.BOOLEAN, true), Set.copyOf(before), Set.of(), Set.copyOf(before), List.of(
                    new DashAbilityPower(
                            functionApplication(LogicVFunctions.AND, Map.of(
                                    "operands", list(List.of(
                                            functionApplication(StandardVFunctions.LESS_THAN_OR_EQUAL, Map.of(
                                                    "a", functionApplication(Actions.GET_COOLDOWN, Map.of("entity", variable("entity"), "cooldown_id", value(DTypes.RESOURCE_LOCATION, cooldownId))),
                                                    "b", value(StandardVTypes.NUMBER, 0.0)
                                            )),
                                            functionApplication(StandardVFunctions.GREATER_THAN_OR_EQUAL, Map.of(
                                                    "a", functionApplication(HaemaDFunctions.BLOOD, Map.of("entity", variable("entity"))),
                                                    "b", value(StandardVTypes.NUMBER, 18.0)
                                            )))))),
                        list(List.of(
                                object(Actions.name(Actions.SET_COOLDOWN), Map.of(
                                        "entity", variable("entity"),
                                        "cooldown_id", value(DTypes.RESOURCE_LOCATION, cooldownId),
                                        "length", value(StandardVTypes.NUMBER, (double) cooldown))))),
                        List.of("key.haema.primary_vampire_action"))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("dash/"+level));
            entries.add(key, dashAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createMistFormAbility(Entries entries) {
            var mistFormAbility = new VampireAbility(true, IconProvider.of(id("textures/mob_effect/mist_form.png"), 11), true, value(StandardVTypes.BOOLEAN, true), Set.of(), Set.of(), Set.of(), List.of(
                    /* TODO */
            ));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("mist_form"));
            entries.add(key, mistFormAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createInvisibilityAbility(Entries entries, int level, Collection<ResourceKey<VampireAbility>> before) {
            var invisibilityAbility = new VampireAbility(true, IconProvider.of(new ResourceLocation("textures/mob_effect/invisibility.png")), true, value(StandardVTypes.BOOLEAN, true), Set.copyOf(before), Set.of(), Set.copyOf(before), List.of(
                    /* TODO */
            ));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("invisibility/"+level));
            entries.add(key, invisibilityAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createSleepInDayAbility(Entries entries) {
            var sleepInDayAbility = new VampireAbility(true, IconProvider.of(Items.RED_BED), false, value(StandardVTypes.BOOLEAN, true), Set.of(), Set.of(), Set.of(), List.of(
                    new SleepInDayAbilityPower()
            ));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("sleep_in_day"));
            entries.add(key, sleepInDayAbility);
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
        public void buildRecipes(RecipeOutput exporter) {
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
