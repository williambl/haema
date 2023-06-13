package com.williambl.haema.api.ritual.module;

import com.mojang.serialization.Codec;
import com.williambl.haema.api.ritual.RitualArae;
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.util.KeyDispatchDataCodec;

import java.util.function.Function;

import static com.williambl.haema.Haema.id;

public interface AraeModule {
    ResourceKey<Registry<Codec<? extends AraeModule>>> RESOURCE_KEY
            = ResourceKey.createRegistryKey(id("arae_module"));
    Registry<Codec<? extends AraeModule>> REGISTRY
            = FabricRegistryBuilder.createSimple(RESOURCE_KEY).buildAndRegister();

    Codec<AraeModule> MODULE_CODEC = REGISTRY.byNameCodec().dispatch(
            p -> p.codec().codec(),
            Function.identity()
    );

    void onAraeCreated(RitualArae arae);
    void onAraeTick(RitualArae arae);
    //void onRitualComplete(RitualArae arae, Ritual ritual); TODO after rituals are implemented
    void onAraeDestroyed(RitualArae arae);

    KeyDispatchDataCodec<? extends AraeModule> codec();
}
