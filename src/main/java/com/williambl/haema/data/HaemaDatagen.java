package com.williambl.haema.data;

import com.williambl.dpred.Comparison;
import com.williambl.dpred.EntityDPredicates;
import com.williambl.dpred.LevelDPredicates;
import com.williambl.dpred.NumberDPredicates;
import com.williambl.haema.Haema;
import com.williambl.haema.HaemaDPredicates;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.vampire.HaemaVampires;
import com.williambl.haema.vampire.ability.powers.HealingVampireAbilityPower;
import net.fabricmc.fabric.api.datagen.v1.DataGeneratorEntrypoint;
import net.fabricmc.fabric.api.datagen.v1.FabricDataGenerator;
import net.fabricmc.fabric.api.datagen.v1.FabricDataOutput;
import net.fabricmc.fabric.api.datagen.v1.provider.FabricDynamicRegistryProvider;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.RegistrySetBuilder;
import net.minecraft.resources.ResourceKey;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;
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
            var healingAbilityPower = new HealingVampireAbilityPower(EntityDPredicates.AND.factory().apply(List.of(
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
                            ))
                    ))
            )));
            var healingAbility = new VampireAbility(true, false, Set.of(), Set.of(), List.of(healingAbilityPower));
            entries.add(ResourceKey.create(VampireAbility.REGISTRY_KEY, id("healing")), healingAbility);
            entries.add(HaemaVampires.VampirismSources.BLOOD_INJECTOR, new VampirismSource(Set.of(HaemaVampires.VampirismSources.BLOOD_INJECTOR), Set.of()));
            entries.add(HaemaVampires.VampirismSources.COMMAND, new VampirismSource(Set.of(HaemaVampires.VampirismSources.COMMAND), Set.of()));
        }

        @Override
        public String getName() {
            return "Dynamic Registry Objects";
        }
    }
}
