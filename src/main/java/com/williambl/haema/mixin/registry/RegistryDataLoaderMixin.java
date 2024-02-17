package com.williambl.haema.mixin.registry;

import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.api.ritual.ritual.Ritual;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import net.minecraft.resources.RegistryDataLoader;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Mutable;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.ArrayList;
import java.util.List;

@Mixin(RegistryDataLoader.class)
public class RegistryDataLoaderMixin {

    @Shadow @Mutable @Final public static List<RegistryDataLoader.RegistryData<?>> WORLDGEN_REGISTRIES;

    @Inject(method = "<clinit>", at = @At("TAIL"))
    private static void haema$registerVampireRegistries(CallbackInfo ci) {
        WORLDGEN_REGISTRIES = new ArrayList<>(WORLDGEN_REGISTRIES);
        WORLDGEN_REGISTRIES.add(new RegistryDataLoader.RegistryData<>(VampireAbility.REGISTRY_KEY, VampireAbility.CODEC));
        WORLDGEN_REGISTRIES.add(new RegistryDataLoader.RegistryData<>(VampirismSource.REGISTRY_KEY, VampirismSource.CODEC));
        WORLDGEN_REGISTRIES.add(new RegistryDataLoader.RegistryData<>(RitualArae.REGISTRY_KEY, RitualArae.CODEC));
        WORLDGEN_REGISTRIES.add(new RegistryDataLoader.RegistryData<>(Ritual.REGISTRY_KEY, Ritual.CODEC));
    }
}
