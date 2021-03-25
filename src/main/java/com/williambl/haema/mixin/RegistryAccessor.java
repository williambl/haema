package com.williambl.haema.mixin;

import net.minecraft.util.registry.DefaultedRegistry;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.registry.RegistryKey;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Invoker;

import java.util.function.Supplier;

@Mixin(Registry.class)
public interface RegistryAccessor {
    @Invoker("createRegistryKey")
    static <T> RegistryKey<Registry<T>> createRegistryKey(String registryId) {
        throw new UnsupportedOperationException();
    }

    @Invoker("create")
    static <T> DefaultedRegistry<T> create(RegistryKey<? extends Registry<T>> registryKey, String defaultId, Supplier<T> defaultEntry) {
        throw new UnsupportedOperationException();
    }
}
