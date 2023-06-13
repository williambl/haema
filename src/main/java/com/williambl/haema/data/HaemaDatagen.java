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
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodBottleItem;
import com.williambl.haema.content.injector.BloodFillingRecipe;
import com.williambl.haema.content.injector.InjectorItem;
import com.williambl.haema.ritual.HaemaRituals;
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
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.models.BlockModelGenerators;
import net.minecraft.data.models.ItemModelGenerators;
import net.minecraft.data.models.blockstates.MultiVariantGenerator;
import net.minecraft.data.models.blockstates.PropertyDispatch;
import net.minecraft.data.models.blockstates.Variant;
import net.minecraft.data.models.blockstates.VariantProperties;
import net.minecraft.data.models.model.ModelLocationUtils;
import net.minecraft.data.models.model.ModelTemplate;
import net.minecraft.data.models.model.ModelTemplates;
import net.minecraft.data.models.model.TextureMapping;
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
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.LayeredCauldronBlock;
import net.minecraft.world.level.levelgen.blockpredicates.BlockPredicate;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

import static com.williambl.haema.Haema.id;

public class HaemaDatagen implements DataGeneratorEntrypoint {
    @Override
    public void onInitializeDataGenerator(FabricDataGenerator fabricDataGenerator) {
        var pack = fabricDataGenerator.createPack();
        pack.addProvider(HaemaDynamicRegistryProvider::new);
        pack.addProvider(HaemaBlockTagsProvider::new);
        pack.addProvider((o, r) -> new HaemaItemTagsProvider(o, r, null));
        pack.addProvider(HaemaEntityTagsProvider::new);
        pack.addProvider(HaemaDamageTypeTagsProvider::new);
        pack.addProvider(HaemaLangProvider::new);
        pack.addProvider(HaemaModelProvider::new);
        pack.addProvider(HaemaRecipeProvider::new);
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
    }

    private static class HaemaModelProvider extends FabricModelProvider {

        public HaemaModelProvider(FabricDataOutput output) {
            super(output);
        }

        @Override
        public void generateBlockStateModels(BlockModelGenerators models) {
            for (var entry : HaemaContent.Fluids.BLOOD_CAULDRON.entrySet()) {
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
            models.generateFlatItem(HaemaContent.Items.EMPTY_INJECTOR, ModelTemplates.FLAT_ITEM);
            for (var item : HaemaContent.Items.INJECTORS.values()) {
                generateFlatItem(item, id("item/full_injector"), ModelTemplates.FLAT_ITEM, models);
            }
            for (var item : HaemaContent.Items.BUCKETS.values()) {
                models.generateFlatItem(item, Items.WATER_BUCKET, ModelTemplates.FLAT_ITEM); //TODO use own texture
            }
            for (var item : HaemaContent.Items.BOTTLES.values()) {
                generateFlatItem(item, id("item/blood_bottle"), ModelTemplates.FLAT_ITEM, models);
            }
        }

        public final void generateFlatItem(Item item, ResourceLocation texture, ModelTemplate modelTemplate, ItemModelGenerators models) {
            modelTemplate.create(ModelLocationUtils.getModelLocation(item), TextureMapping.layer0(texture), models.output);
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
            for (var cauldron : HaemaContent.Fluids.BLOOD_CAULDRON.values()) {
                cauldrons.add(cauldron);
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


            entries.add(HaemaContent.VampirismSources.BLOOD_INJECTOR, new VampirismSource(Set.of(HaemaContent.VampirismSources.BLOOD_INJECTOR, HaemaVampires.VampirismSources.COMMAND), defaultAbilties, DPredicates.CONSTANT.factory().apply(false), DPredicates.CONSTANT.factory().apply(false))); //TODO
            entries.add(HaemaVampires.VampirismSources.COMMAND, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of(), DPredicates.CONSTANT.factory().apply(true), DPredicates.CONSTANT.factory().apply(true)));

            entries.add(ResourceKey.create(RitualArae.REGISTRY_KEY, id("test")), new RitualArae(new MultiblockFilter(
                    new char[][][]{
                            new char[][] {
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
                            }
                    },
                    'r',
                    Map.of(
                            'c', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(Blocks.CRIMSON_PLANKS), ContextArg.BLOCK.arg()),
                            'C', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(Blocks.CANDLE), ContextArg.BLOCK.arg()),
                            'r', BlockInWorldDFunctions.BLOCK_PREDICATE.factory().apply(BlockPredicate.matchesBlocks(HaemaRituals.RitualBlocks.RITUAL_ALTAR), ContextArg.BLOCK.arg())),
                    DPredicates.CONSTANT.factory().apply(true)
            ), List.of()));
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
            translations.add(Haema.TAB, "Haema");
            for (var block : HaemaContent.Fluids.BLOOD_BLOCK.values()) {
                translations.add(block, "%1$s Blood");
            }
            for (var cauldron : HaemaContent.Fluids.BLOOD_CAULDRON.values()) {
                translations.add(cauldron, "Blood Cauldron");
            }
            translations.add(HaemaContent.Items.EMPTY_INJECTOR, "Empty Blood Injector");
            for (var item : HaemaContent.Items.INJECTORS.values()) {
                translations.add(item, "Blood Injector");
            }
            translations.add(InjectorItem.DESCRIPTION_TRANSLATION_KEY, "Contains %1$s blood");
            for (var item : HaemaContent.Items.BUCKETS.values()) {
                translations.add(item, "%1$s Blood Bucket");
            }
            for (var item : HaemaContent.Items.BOTTLES.values()) {
                translations.add(item, "Blood Bottle");
            }
            translations.add(BloodBottleItem.DESCRIPTION_TRANSLATION_KEY, "Contains %1$s blood");
            for (var quality : BloodQuality.values()) {
                translations.add(quality.translationKey, quality.getSerializedName().substring(0, 1).toUpperCase(Locale.ROOT) + quality.getSerializedName().substring(1));
            }
        }
    }

    public static class HaemaRecipeProvider extends FabricRecipeProvider {

        public HaemaRecipeProvider(FabricDataOutput output) {
            super(output);
        }

        @Override
        public void buildRecipes(Consumer<FinishedRecipe> exporter) {
            BloodFillingRecipe.Builder.create()
                    .empty(Ingredient.of(HaemaContent.Items.EMPTY_INJECTOR))
                    .save(exporter, id("injector_filling"));
            BloodFillingRecipe.Builder.create()
                    .empty(Ingredient.of(Items.GLASS_BOTTLE))
                    .save(exporter, id("bottle_filling"));
        }
    }
}
