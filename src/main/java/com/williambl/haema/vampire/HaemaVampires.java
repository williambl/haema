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
import com.williambl.haema.vampire.ability.powers.HealingVampireAbilityPower;
import com.williambl.haema.vampire.ability.powers.damage_modification.DamageModificationAbilityPower;
import com.williambl.haema.vampire.ability.powers.sunlight_sickness.SunlightSicknessEffect;
import com.williambl.haema.vampire.ability.powers.vampiric_weakness.VampiricWeaknessEffect;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry;
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.gamerule.v1.CustomGameRuleCategory;
import net.fabricmc.fabric.api.gamerule.v1.GameRuleFactory;
import net.fabricmc.fabric.api.gamerule.v1.GameRuleRegistry;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.GameRules;

import static com.williambl.haema.Haema.id;

public class HaemaVampires {
    public static void init() {
        ServerLifecycleEvents.END_DATA_PACK_RELOAD.register((server, resources, success) -> {
            server.registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY).holders().forEach(holder -> {
                Haema.LOGGER.info("Vampire ability: {}\nContents: {}", holder.key().location(), holder.value());
            });
        });
        VampireMobEffects.init();
        VampireAbilityPowers.init();
        VampirismSources.init();
        VampireGameRules.init();
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
        public static final Codec<HealingVampireAbilityPower> HEALING_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("healing"), HealingVampireAbilityPower.CODEC.codec());
        public static final Codec<DamageModificationAbilityPower> DAMAGE_MODIFICATION_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("damage_modification"), DamageModificationAbilityPower.CODEC.codec());

        private static void init() {
            DamageModificationAbilityPower.init();
        }
    }

    public static class VampirismSources {
        public static final ResourceKey<VampirismSource> BLOOD_INJECTOR = ResourceKey.create(VampirismSource.REGISTRY_KEY, id("blood_injector"));
        public static final ResourceKey<VampirismSource> COMMAND = ResourceKey.create(VampirismSource.REGISTRY_KEY, id("command"));

        private static void init() {}
    }

    public static class VampireMobEffects {
        public static final SunlightSicknessEffect SUNLIGHT_SICKNESS = Registry.register(BuiltInRegistries.MOB_EFFECT, id("sunlight_sickness"), new SunlightSicknessEffect());
        public static final VampiricWeaknessEffect VAMPIRIC_WEAKNESS = Registry.register(BuiltInRegistries.MOB_EFFECT, id("vampiric_weakness"), new VampiricWeaknessEffect());

        private static void init() {}
    }

    public static class VampireGameRules {
        public static final CustomGameRuleCategory HAEMA_CATEGORY = new CustomGameRuleCategory(id("haema"), Component.translatable("gamerule.category.haema"));
        public static final GameRules.Key<GameRules.BooleanValue> VAMPIRES_BURN = GameRuleRegistry.register(id("vampires_burn").toString(), HAEMA_CATEGORY, GameRuleFactory.createBooleanRule(true));

        private static void init() {}
    }
}
