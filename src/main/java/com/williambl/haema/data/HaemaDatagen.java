package com.williambl.haema.data;

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes;
import com.williambl.dfunc.api.Comparison;
import com.williambl.dfunc.api.context.ContextArg;
import com.williambl.dfunc.api.functions.DPredicates;
import com.williambl.dfunc.api.functions.EntityDFunctions;
import com.williambl.dfunc.api.functions.LevelDFunctions;
import com.williambl.dfunc.api.functions.NumberDFunctions;
import com.williambl.haema.Haema;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.vampire.HaemaVampires;
import com.williambl.haema.vampire.ability.powers.AttributeVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.EffectVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.HealingVampireAbilityPower;
import net.fabricmc.fabric.api.datagen.v1.DataGeneratorEntrypoint;
import net.fabricmc.fabric.api.datagen.v1.FabricDataGenerator;
import net.fabricmc.fabric.api.datagen.v1.FabricDataOutput;
import net.fabricmc.fabric.api.datagen.v1.provider.FabricDynamicRegistryProvider;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.RegistrySetBuilder;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static com.williambl.haema.Haema.id;

public class HaemaDatagen implements DataGeneratorEntrypoint {
    @Override
    public void onInitializeDataGenerator(FabricDataGenerator fabricDataGenerator) {
        fabricDataGenerator.createPack().addProvider(HaemaDynamicRegistryProvider::new);
    }

    @Override
    public @Nullable String getEffectiveModId() {
        return Haema.MODID;
    }

    @Override
    public void buildRegistry(RegistrySetBuilder registryBuilder) {
        registryBuilder.add(VampirismSource.REGISTRY_KEY, $ -> {});
        registryBuilder.add(VampireAbility.REGISTRY_KEY, $ -> {});
    }

    @SuppressWarnings("UnstableApiUsage")
    private static class HaemaDynamicRegistryProvider extends FabricDynamicRegistryProvider {

        public HaemaDynamicRegistryProvider(FabricDataOutput output, CompletableFuture<HolderLookup.Provider> registriesFuture) {
            super(output, registriesFuture);
        }

        @Override
        protected void configure(HolderLookup.Provider registries, Entries entries) {
            var healingAbility = this.createHealingAbility(entries);
            var reachAbility = this.createReachAbility(entries);
            var healthBoostAbility = this.createHealthBoostAbility(entries);
            var sunlightSicknessAbility = this.createSunlightSicknessAbility(entries);
            var vampiricWeaknessAbility = this.createVampiricWeaknessAbility(entries);
            entries.add(HaemaVampires.VampirismSources.BLOOD_INJECTOR, new VampirismSource(Set.of(HaemaVampires.VampirismSources.BLOOD_INJECTOR), Set.of(healingAbility, reachAbility, healthBoostAbility, sunlightSicknessAbility, vampiricWeaknessAbility)));
            entries.add(HaemaVampires.VampirismSources.COMMAND, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of()));
        }

        private ResourceKey<VampireAbility> createHealingAbility(Entries entries) {
            var healingAbility = new VampireAbility(true, DPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(
                    new HealingVampireAbilityPower(DPredicates.AND.factory().apply(List.of(
                            LevelDFunctions.BOOLEAN_GAME_RULE.factory().apply("naturalRegeneration", ContextArg.LEVEL.arg()),
                            DPredicates.NOT.factory().apply(EntityDFunctions.DEAD_OR_DYING.factory().apply(ContextArg.ENTITY.arg())),
                            NumberDFunctions.COMPARISON.factory().apply(ContextArg.NUMBER_A.arg(NumberDFunctions.MODULO.factory().apply(ContextArg.NUMBER_A.arg(EntityDFunctions.AGE.factory().apply(ContextArg.ENTITY.arg())), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(20.0)))), ContextArg.NUMBER_B.arg(NumberDFunctions.CONSTANT.factory().apply(0.)), Comparison.EQUAL),
                            //TODO test sunlight sickness
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

        @Override
        public String getName() {
            return "Dynamic Registry Objects";
        }
    }
}
