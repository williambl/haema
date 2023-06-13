package com.williambl.haema.api.ritual.module;

import com.mojang.serialization.Codec;
import com.williambl.haema.api.ritual.RitualArae;
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.level.Level;

import java.util.function.Function;

import static com.williambl.haema.Haema.id;

/**
 * A behaviour module for a ritual arae. These allow for custom behaviour to be added to ritual araes.
 */
public interface AraeModule {
    ResourceKey<Registry<Codec<? extends AraeModule>>> RESOURCE_KEY
            = ResourceKey.createRegistryKey(id("arae_module"));
    Registry<Codec<? extends AraeModule>> REGISTRY
            = FabricRegistryBuilder.createSimple(RESOURCE_KEY).buildAndRegister();

    Codec<AraeModule> MODULE_CODEC = REGISTRY.byNameCodec().dispatch(
            p -> p.codec().codec(),
            Function.identity()
    );

    /**
     * Called when the arae is created.
     * @param arae  the arae
     * @param level the level
     * @param pos   the altar pos
     */
    void onAraeCreated(RitualArae arae, Level level, BlockPos pos);

    /**
     * Called every tick while the arae exists.
     * @param arae  the arae
     * @param level the level
     * @param pos   the altar pos
     */
    void onAraeTick(RitualArae arae, Level level, BlockPos pos);
    //void onRitualComplete(RitualArae arae, Ritual ritual); TODO after rituals are implemented

    /**
     * Called when the arae is destroyed.
     * @param arae  the arae
     * @param level the level
     * @param pos   the altar pos
     */
    void onAraeDestroyed(RitualArae arae, Level level, BlockPos pos);

    /**
     * Get the codec for this module.
     * @return  the codec for this module
     */
    KeyDispatchDataCodec<? extends AraeModule> codec();
}
