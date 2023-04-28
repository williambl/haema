package com.williambl.haema.data;

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes;
import com.williambl.dpred.Comparison;
import com.williambl.dpred.EntityDPredicates;
import com.williambl.dpred.LevelDPredicates;
import com.williambl.dpred.NumberDPredicates;
import com.williambl.haema.Haema;
import com.williambl.haema.HaemaDPredicates;
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
            entries.add(HaemaVampires.VampirismSources.BLOOD_INJECTOR, new VampirismSource(Set.of(HaemaVampires.VampirismSources.BLOOD_INJECTOR), Set.of(healingAbility, reachAbility, healthBoostAbility, sunlightSicknessAbility)));
            entries.add(HaemaVampires.VampirismSources.COMMAND, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of()));
        }

        private ResourceKey<VampireAbility> createHealingAbility(Entries entries) {
            var healingAbility = new VampireAbility(true, EntityDPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(
                    new HealingVampireAbilityPower(EntityDPredicates.AND.factory().apply(List.of(
                            EntityDPredicates.LEVEL_PREDICATE.factory().apply(LevelDPredicates.BOOLEAN_GAME_RULE.factory().apply("naturalRegeneration")),
                            EntityDPredicates.NOT.factory().apply(EntityDPredicates.DEAD_OR_DYING.factory().get()),
                            EntityDPredicates.AGE.factory().apply(NumberDPredicates.MODULO.factory().apply(20.0, NumberDPredicates.COMPARISON.factory().apply(Comparison.EQUAL, 0.0))),
                            //TODO test sunlight sickness
                            HaemaDPredicates.HEALTH_RELATIVE_TO_MAX.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.LESS_THAN, 0.0), true),
                            EntityDPredicates.OR.factory().apply(List.of(
                                    EntityDPredicates.AND.factory().apply(List.of(
                                            HaemaDPredicates.BLOOD.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.GREATER_THAN_OR_EQUAL, 19.0)),
                                            HaemaDPredicates.HEALTH_RELATIVE_TO_MAX.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.LESS_THAN, 20.0), false)
                                    )),
                                    EntityDPredicates.AND.factory().apply(List.of(
                                            HaemaDPredicates.BLOOD.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.GREATER_THAN_OR_EQUAL, 14.0)),
                                            HaemaDPredicates.HEALTH_RELATIVE_TO_MAX.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.LESS_THAN, 10.0), false)
                                    )),
                                    EntityDPredicates.AND.factory().apply(List.of(
                                            HaemaDPredicates.BLOOD.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.GREATER_THAN_OR_EQUAL, 10.0)),
                                            HaemaDPredicates.HEALTH_RELATIVE_TO_MAX.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.LESS_THAN, 6.0), false)
                                    )),
                                    EntityDPredicates.AND.factory().apply(List.of(
                                            HaemaDPredicates.BLOOD.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.GREATER_THAN_OR_EQUAL, 8.0)),
                                            HaemaDPredicates.HEALTH_RELATIVE_TO_MAX.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.LESS_THAN, 0.0), false)
                                    )))))),
                            1f,
                            new double[] { 1.05, 0.0, -1.0 }
                            )));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("healing"));
            entries.add(key, healingAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createReachAbility(Entries entries) {
            var reachAbility = new VampireAbility(true, EntityDPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(new AttributeVampireAbilityPower(Set.of(
                    new AttributeVampireAbilityPower.Data(
                            ReachEntityAttributes.REACH,
                            new AttributeModifier(UUID.fromString("0eb4fc5f-71d5-4440-b517-bcc18e1df6f4"), "Vampire Reach bonus", 2.0, AttributeModifier.Operation.ADDITION),
                            HaemaDPredicates.BLOOD.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.GREATER_THAN_OR_EQUAL, 6.0))
                    ),
                    new AttributeVampireAbilityPower.Data(
                            ReachEntityAttributes.ATTACK_RANGE,
                            new AttributeModifier(UUID.fromString("3267a46b-2b48-429f-a3a8-439aa87a876d"), "Vampire Attack Range bonus", 2.0, AttributeModifier.Operation.ADDITION),
                            HaemaDPredicates.BLOOD.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.GREATER_THAN_OR_EQUAL, 6.0))
                    )
            ))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("reach"));
            entries.add(key, reachAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createHealthBoostAbility(Entries entries) {
            var healthBoostAbility = new VampireAbility(true, EntityDPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(new AttributeVampireAbilityPower(Set.of(
                    new AttributeVampireAbilityPower.Data(
                            Attributes.MAX_HEALTH,
                            new AttributeModifier(UUID.fromString("858a6a28-5092-49ea-a94e-eb74db018a92"), "Vampire Max Health bonus", 1.0, AttributeModifier.Operation.MULTIPLY_BASE),
                            HaemaDPredicates.BLOOD.factory().apply(NumberDPredicates.COMPARISON.factory().apply(Comparison.GREATER_THAN_OR_EQUAL, 3.0))
                    )
            ))));
            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("health_boost"));
            entries.add(key, healthBoostAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createSunlightSicknessAbility(Entries entries) {
            var sunlightSicknessAbility = new VampireAbility(true, EntityDPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(new EffectVampireAbilityPower(Set.of(
                    new EffectVampireAbilityPower.Data(
                            HaemaVampires.VampireMobEffects.SUNLIGHT_SICKNESS,
                            0,
                            10,
                            false,
                            true,
                            true,
                            EntityDPredicates.AND.factory().apply(List.of(
                                    EntityDPredicates.OR.factory().apply(List.of(
                                            EntityDPredicates.LEVEL_PREDICATE.factory().apply(LevelDPredicates.AND.factory().apply(List.of(
                                                    LevelDPredicates.IS_DAY.factory().get(),
                                                    LevelDPredicates.NOT.factory().apply(LevelDPredicates.IS_RAINING.factory().get())))),
                                            HaemaDPredicates.TRIGGER_BURN_EVENT.factory().get())),
                                    EntityDPredicates.AND.factory().apply(List.of(
                                            EntityDPredicates.LEVEL_PREDICATE.factory().apply(LevelDPredicates.BOOLEAN_GAME_RULE.factory().apply(HaemaVampires.VampireGameRules.VAMPIRES_BURN.getId())),
                                            EntityDPredicates.IS_SURVIVAL_LIKE.factory().get(),
                                            EntityDPredicates.NOT.factory().apply(HaemaDPredicates.PREVENT_BURN_EVENT.factory().get()))))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("sunlight_sickness"));
            entries.add(key, sunlightSicknessAbility);
            return key;
        }

        private ResourceKey<VampireAbility> createVampiricWeaknessAbility(Entries entries) {
            var sunlightSicknessAbility = new VampireAbility(true, EntityDPredicates.CONSTANT.factory().apply(false), Set.of(), Set.of(), Set.of(), List.of(new EffectVampireAbilityPower(Set.of(
                    new EffectVampireAbilityPower.Data(
                            HaemaVampires.VampireMobEffects.VAMPIRIC_WEAKNESS,
                            0,
                            10,
                            false,
                            true,
                            true,
                            EntityDPredicates.AND.factory().apply(List.of(
                                    EntityDPredicates.OR.factory().apply(List.of(
                                            EntityDPredicates.LEVEL_PREDICATE.factory().apply(LevelDPredicates.AND.factory().apply(List.of(
                                                    LevelDPredicates.IS_DAY.factory().get(),
                                                    LevelDPredicates.NOT.factory().apply(LevelDPredicates.IS_RAINING.factory().get())))),
                                            HaemaDPredicates.TRIGGER_BURN_EVENT.factory().get())),
                                    EntityDPredicates.AND.factory().apply(List.of(
                                            EntityDPredicates.LEVEL_PREDICATE.factory().apply(LevelDPredicates.BOOLEAN_GAME_RULE.factory().apply(HaemaVampires.VampireGameRules.VAMPIRES_BURN.getId())),
                                            EntityDPredicates.IS_SURVIVAL_LIKE.factory().get(),
                                            EntityDPredicates.NOT.factory().apply(HaemaDPredicates.PREVENT_BURN_EVENT.factory().get()))))))))));

            var key = ResourceKey.create(VampireAbility.REGISTRY_KEY, id("sunlight_sickness"));
            entries.add(key, sunlightSicknessAbility);
            return key;

        }

        @Override
        public String getName() {
            return "Dynamic Registry Objects";
        }
    }
}
