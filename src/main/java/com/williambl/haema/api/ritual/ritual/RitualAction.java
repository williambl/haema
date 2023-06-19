package com.williambl.haema.api.ritual.ritual;

import com.mojang.serialization.Codec;
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.util.KeyDispatchDataCodec;

import java.util.function.Function;

import static com.williambl.haema.Haema.id;

public interface RitualAction {
    ResourceKey<Registry<Codec<? extends RitualAction>>> RESOURCE_KEY
            = ResourceKey.createRegistryKey(id("ritual_action"));
    Registry<Codec<? extends RitualAction>> REGISTRY
            = FabricRegistryBuilder.createSimple(RESOURCE_KEY).buildAndRegister();

    Codec<RitualAction> ACTION_CODEC = REGISTRY.byNameCodec().dispatch(
            p -> p.codec().codec(),
            Function.identity()
    );

    void run(Ritual ritual, RitualContainer container);

    KeyDispatchDataCodec<? extends RitualAction> codec();
}
