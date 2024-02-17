package com.williambl.haema.client.mixin;

import net.minecraft.client.KeyMapping;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

import java.util.Map;

@Mixin(KeyMapping.class)
public interface KeyMappingAccessor {
    @Accessor
    static Map<String, KeyMapping> getALL() {
        throw new UnsupportedOperationException();
    }
}
