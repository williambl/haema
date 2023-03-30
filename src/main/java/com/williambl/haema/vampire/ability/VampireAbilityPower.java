package com.williambl.haema.vampire.ability;

import com.mojang.serialization.Codec;
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

import java.util.function.Function;

import static com.williambl.haema.Haema.id;

/**
 * A power that is applied to a player when they have a vampire ability.
 * <p>
 * To create a new ability, first create a new class that implements this interface.
 * Then register the codec for your class in {@link VampireAbilityPower#REGISTRY the registry}.
 * @see VampireAbility
 */
public interface VampireAbilityPower {
    ResourceKey<Registry<Codec<? extends VampireAbilityPower>>> RESOURCE_KEY
            = ResourceKey.createRegistryKey(id("vampire_ability_power"));
    Registry<Codec<? extends VampireAbilityPower>> REGISTRY
            = FabricRegistryBuilder.createSimple(RESOURCE_KEY).buildAndRegister();

    Codec<VampireAbilityPower> POWER_CODEC = REGISTRY.byNameCodec().dispatch(
            p -> p.codec().codec(),
            Function.identity()
    );

    /**
     * Apply this power to the given entity.
     * @param entity    the entity to apply the power to
     * @param source    the ability that this power is being applied for
     */
    void apply(LivingEntity entity, VampireAbility source);

    /**
     * Remove this power from the given entity.
     * @param entity    the entity to remove the power from
     * @param source    the ability that this power is being removed for
     */
    void remove(LivingEntity entity, VampireAbility source);

    /**
     * Get the codec for this power.
     * @return  the codec for this power
     */
    KeyDispatchDataCodec<? extends VampireAbilityPower> codec();
}
