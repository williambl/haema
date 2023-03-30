package com.williambl.haema.vampire;

import com.mojang.serialization.Codec;
import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.AttributeVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.DummyVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.EffectVampireAbilityPower;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.minecraft.core.Registry;

import static com.williambl.haema.Haema.id;

public class HaemaVampires {
    public static void init() {
        ServerLifecycleEvents.END_DATA_PACK_RELOAD.register((server, resources, success) -> {
            server.registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY).holders().forEach(holder -> {
                Haema.LOGGER.info("Vampire ability: {}\nContents: {}", holder.key().location(), holder.value());
            });
        });
        VampireAbilityPowers.init();
    }

    public static class VampireAbilityPowers {
        //public static final Codec<ApoliVampireAbilityPower> APOLI_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("apoli"), ApoliVampireAbilityPower.CODEC);
        public static final Codec<DummyVampireAbilityPower> DUMMY_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("apoli"), DummyVampireAbilityPower.CODEC.codec());
        public static final Codec<AttributeVampireAbilityPower> ATTRIBUTE_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("attribute"), AttributeVampireAbilityPower.CODEC.codec());
        public static final Codec<EffectVampireAbilityPower> EFFECT_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("effect"), EffectVampireAbilityPower.CODEC.codec());

        private static void init() {}
    }
}
