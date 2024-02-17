package com.williambl.haema.vampire;

import com.mojang.serialization.Codec;
import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.haema.vampire.ability.SetActiveAbilityPacket;
import com.williambl.haema.vampire.ability.abilities.strength.VampiricStrengthEffect;
import com.williambl.haema.vampire.ability.powers.*;
import com.williambl.haema.vampire.ability.powers.damage_modification.DamageModificationAbilityPower;
import com.williambl.haema.vampire.ability.powers.dash.DashAbilityPower;
import com.williambl.haema.vampire.ability.powers.dash.DashPacketC2S;
import com.williambl.haema.vampire.ability.powers.dash.EntityChargingDashComponent;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingPacket;
import com.williambl.haema.vampire.ability.powers.hungerbar.ModifyHungerBarAbilityPower;
import com.williambl.haema.vampire.ability.powers.sleep.SleepInDayAbilityPower;
import com.williambl.haema.vampire.ability.powers.sunlight_sickness.SunlightSicknessEffect;
import com.williambl.haema.vampire.ability.powers.vampiric_weakness.VampiricWeaknessEffect;
import com.williambl.haema.vampire.ability.powers.vision.VampireVisionVampireAbilityPower;
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry;
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.gamerule.v1.CustomGameRuleCategory;
import net.fabricmc.fabric.api.gamerule.v1.GameRuleFactory;
import net.fabricmc.fabric.api.gamerule.v1.GameRuleRegistry;
import net.fabricmc.fabric.api.networking.v1.FabricPacket;
import net.fabricmc.fabric.api.networking.v1.PacketType;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.tags.TagKey;
import net.minecraft.world.damagesource.DamageType;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.GameRules;

import static com.williambl.haema.Haema.id;

public class HaemaVampires {
    public static void init() {
        ServerLifecycleEvents.END_DATA_PACK_RELOAD.register((server, resources, success) -> {
            server.registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY).holders().forEach(holder -> {
                Haema.LOGGER.info("Vampire ability: {}\nContents: {}", holder.key().location(), holder.value());
            });
        });
        VampirePackets.init();
        VampireMobEffects.init();
        VampireAbilityPowers.init();
        VampirismSources.init();
        VampireGameRules.init();
        VampireTags.init();
    }

    public static void initEntityComponents(EntityComponentFactoryRegistry registry) {
        registry.registerForPlayers(VampireComponent.KEY, EntityVampireComponent::new, RespawnCopyStrategy.CHARACTER);
        registry.registerForPlayers(VampireAbilitiesComponent.KEY, EntityVampireAbilitiesComponent::new, RespawnCopyStrategy.CHARACTER);
        VampireAbilityPowers.initEntityComponents(registry);
    }

    public static class VampirePackets {
        public static final PacketType<DrinkingPacket> DRINKING = PacketType.create(id("drinking"), DrinkingPacket::new);
        public static final PacketType<SetActiveAbilityPacket> SET_ACTIVE_ABILITY = PacketType.create(id("set_active_ability"), SetActiveAbilityPacket::new);
        public static final PacketType<DashPacketC2S> DASH = PacketType.create(id("dash"), DashPacketC2S::new);

        public static void init() {
            DrinkingPacket.init();
            SetActiveAbilityPacket.init();
            DashPacketC2S.init();
        }
    }

    public static class VampireAbilityPowers {
        //public static final Codec<ApoliVampireAbilityPower> APOLI_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("apoli"), ApoliVampireAbilityPower.CODEC);
        public static final Codec<DummyVampireAbilityPower> DUMMY_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("apoli"), DummyVampireAbilityPower.CODEC.codec());
        public static final Codec<AttributeVampireAbilityPower> ATTRIBUTE_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("attribute"), AttributeVampireAbilityPower.CODEC.codec());
        public static final Codec<EffectVampireAbilityPower> EFFECT_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("effect"), EffectVampireAbilityPower.CODEC.codec());
        public static final Codec<HealingVampireAbilityPower> HEALING_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("healing"), HealingVampireAbilityPower.CODEC.codec());
        public static final Codec<DamageModificationAbilityPower> DAMAGE_MODIFICATION_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("damage_modification"), DamageModificationAbilityPower.CODEC.codec());
        public static final Codec<VampireVisionVampireAbilityPower> VAMPIRE_VISION_VAMPIRE_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("vampire_vision"), VampireVisionVampireAbilityPower.CODEC.codec());
        public static final Codec<DashAbilityPower> DASH_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("dash"), DashAbilityPower.CODEC.codec());
        public static final Codec<DrinkingAbilityPower> DRINKING_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("drinking"), DrinkingAbilityPower.CODEC.codec());
        public static final Codec<ModifyHungerBarAbilityPower> MODIFY_HUNGER_BAR_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("modify_hunger_bar"), ModifyHungerBarAbilityPower.CODEC.codec());
        public static final Codec<SleepInDayAbilityPower> SLEEP_IN_DAY_ABILITY_POWER_CODEC = Registry.register(VampireAbilityPower.REGISTRY, id("sleep_in_day"), SleepInDayAbilityPower.CODEC.codec());

        private static void init() {
            DamageModificationAbilityPower.init();
            DashAbilityPower.init();
            DrinkingAbilityPower.init();
            SleepInDayAbilityPower.init();
        }

        public static void initEntityComponents(EntityComponentFactoryRegistry registry) {
            registry.registerForPlayers(EntityChargingDashComponent.KEY, p -> new EntityChargingDashComponent(), RespawnCopyStrategy.NEVER_COPY);
        }
    }

    public static class VampirismSources {
        public static final ResourceKey<VampirismSource> COMMAND = ResourceKey.create(VampirismSource.REGISTRY_KEY, id("command"));

        private static void init() {}
    }

    public static class VampireMobEffects {
        public static final SunlightSicknessEffect SUNLIGHT_SICKNESS = Registry.register(BuiltInRegistries.MOB_EFFECT, id("sunlight_sickness"), new SunlightSicknessEffect());
        public static final VampiricWeaknessEffect VAMPIRIC_WEAKNESS = Registry.register(BuiltInRegistries.MOB_EFFECT, id("vampiric_weakness"), new VampiricWeaknessEffect());
        public static final VampiricStrengthEffect VAMPIRIC_STRENGTH = Registry.register(BuiltInRegistries.MOB_EFFECT, id("vampiric_strength"), new VampiricStrengthEffect());

        private static void init() {}
    }

    public static class VampireGameRules {
        public static final CustomGameRuleCategory HAEMA_CATEGORY = new CustomGameRuleCategory(id("haema"), Component.translatable("gamerule.category.haema"));
        public static final GameRules.Key<GameRules.BooleanValue> VAMPIRES_BURN = GameRuleRegistry.register(id("vampires_burn").toString(), HAEMA_CATEGORY, GameRuleFactory.createBooleanRule(true));

        private static void init() {}
    }

    public static class VampireTags {
        public static final TagKey<DamageType> VAMPIRE_EFFECTIVE_DAMAGE = TagKey.create(Registries.DAMAGE_TYPE, id("vampire_effective"));
        public static final TagKey<Item> VAMPIRE_EFFECTIVE_WEAPONS = TagKey.create(Registries.ITEM, id("vampire_effective_weapons"));

        private static void init() {}
    }
}
