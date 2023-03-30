package com.williambl.haema.vampire.ability;

import com.mojang.serialization.Codec;
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

import java.util.function.Function;

import static com.williambl.haema.Haema.id;

public interface VampireAbilityPower {
    ResourceKey<Registry<Codec<? extends VampireAbilityPower>>> RESOURCE_KEY
            = ResourceKey.createRegistryKey(id("vampire_ability_power"));
    Registry<Codec<? extends VampireAbilityPower>> REGISTRY
            = FabricRegistryBuilder.createSimple(RESOURCE_KEY).buildAndRegister();

    Codec<VampireAbilityPower> POWER_CODEC = REGISTRY.byNameCodec().dispatch(
            p -> p.codec().codec(),
            Function.identity()
    );

    void apply(LivingEntity entity, VampireAbility source);
    void remove(LivingEntity entity, VampireAbility source);
    KeyDispatchDataCodec<? extends VampireAbilityPower> codec();
}
