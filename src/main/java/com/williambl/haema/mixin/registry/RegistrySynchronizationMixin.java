package com.williambl.haema.mixin.registry;

import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistrySynchronization;
import net.minecraft.resources.ResourceKey;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Mutable;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.HashMap;
import java.util.Map;

@Mixin(RegistrySynchronization.class)
public class RegistrySynchronizationMixin {
    @Shadow @Mutable @Final private static Map<ResourceKey<? extends Registry<?>>, RegistrySynchronization.NetworkedRegistryData<?>> NETWORKABLE_REGISTRIES;

    @Inject(method = "<clinit>", at = @At("TAIL"))
    private static void haema$syncVampireRegistries(CallbackInfo ci) {
        NETWORKABLE_REGISTRIES = new HashMap<>(NETWORKABLE_REGISTRIES);
        NETWORKABLE_REGISTRIES.put(VampireAbility.REGISTRY_KEY, new RegistrySynchronization.NetworkedRegistryData<>(VampireAbility.REGISTRY_KEY, VampireAbility.CODEC));
        NETWORKABLE_REGISTRIES.put(VampirismSource.REGISTRY_KEY, new RegistrySynchronization.NetworkedRegistryData<>(VampirismSource.REGISTRY_KEY, VampirismSource.CODEC));
        NETWORKABLE_REGISTRIES.put(RitualArae.REGISTRY_KEY, new RegistrySynchronization.NetworkedRegistryData<>(RitualArae.REGISTRY_KEY, RitualArae.CODEC));
    }
}
