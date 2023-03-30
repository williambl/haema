package com.williambl.haema.mixin.vampire.ability;

import com.mojang.serialization.Codec;
import com.williambl.haema.vampire.ability.VampireAbility;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
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
    private static void haema$syncVampireAbilityRegistry(CallbackInfo ci) {
        NETWORKABLE_REGISTRIES = new HashMap<>(NETWORKABLE_REGISTRIES);
        NETWORKABLE_REGISTRIES.put(VampireAbility.REGISTRY_KEY, new RegistrySynchronization.NetworkedRegistryData<>(VampireAbility.REGISTRY_KEY, VampireAbility.CODEC));
    }
}
