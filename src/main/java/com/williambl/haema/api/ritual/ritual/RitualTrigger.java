package com.williambl.haema.api.ritual.ritual;

import com.mojang.serialization.Codec;
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.util.KeyDispatchDataCodec;

import java.util.function.Function;

import static com.williambl.haema.Haema.id;

public interface RitualTrigger {
    ResourceKey<Registry<Codec<? extends RitualTrigger>>> RESOURCE_KEY
            = ResourceKey.createRegistryKey(id("ritual_trigger"));
    Registry<Codec<? extends RitualTrigger>> REGISTRY
            = FabricRegistryBuilder.createSimple(RESOURCE_KEY).buildAndRegister();

    Codec<RitualTrigger> TRIGGER_CODEC = REGISTRY.byNameCodec().dispatch(
            p -> p.codec().codec(),
            Function.identity()
    );

    KeyDispatchDataCodec<? extends RitualTrigger> codec();
}
