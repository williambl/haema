package com.williambl.haema.vampire;

import com.mojang.serialization.Codec;
import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.AttributeVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.DummyVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.EffectVampireAbilityPower;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry;
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;

import static com.williambl.haema.Haema.id;

public class HaemaVampires {
    public static void init() {
        ServerLifecycleEvents.END_DATA_PACK_RELOAD.register((server, resources, success) -> {
            server.registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY).holders().forEach(holder -> {
                Haema.LOGGER.info("Vampire ability: {}\nContents: {}", holder.key().location(), holder.value());
            });
        });
        VampireAbilityPowers.init();
        VampirismSources.init();
    }

    public static void initEntityComponents(EntityComponentFactoryRegistry registry) {
        registry.registerForPlayers(VampireComponent.KEY, EntityVampireComponent::new, RespawnCopyStrategy.CHARACTER);
        registry.registerForPlayers(VampireAbilitiesComponent.KEY, EntityVampireAbilitiesComponent::new, RespawnCopyStrategy.CHARACTER);
    }

    public static class VampireAbilityPowers {
        //public static final Codec<ApoliVampireAbilityPower> APOLI_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("apoli"), ApoliVampireAbilityPower.CODEC);
        public static final Codec<DummyVampireAbilityPower> DUMMY_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("apoli"), DummyVampireAbilityPower.CODEC.codec());
        public static final Codec<AttributeVampireAbilityPower> ATTRIBUTE_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("attribute"), AttributeVampireAbilityPower.CODEC.codec());
        public static final Codec<EffectVampireAbilityPower> EFFECT_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("effect"), EffectVampireAbilityPower.CODEC.codec());

        private static void init() {}
    }

    public static class VampirismSources {
        public static final ResourceKey<VampirismSource> BLOOD_INJECTOR = ResourceKey.create(VampirismSource.REGISTRY_KEY, id("blood_injector"));
        public static final ResourceKey<VampirismSource> COMMAND = ResourceKey.create(VampirismSource.REGISTRY_KEY, id("command"));

        private static void init() {}
    }
}
