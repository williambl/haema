package com.williambl.haema.api.content.blood;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.storage.Storage;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import org.jetbrains.annotations.Nullable;

@FunctionalInterface
public interface EntityBloodStorageCallback {
    ResourceLocation VAMPIRE_BLOOD_BACKED = new ResourceLocation("haema:vampire_blood_backed");
    ResourceLocation DEFAULT_HEALTH_BACKED = new ResourceLocation("haema:default_health_backed");

    Event<EntityBloodStorageCallback> EVENT = EventFactory.createWithPhases(EntityBloodStorageCallback.class, callbacks -> (e) -> {
        for (EntityBloodStorageCallback callback : callbacks) {
            var result = callback.findStorage(e);
            if (result != null) {
                return result;
            }
        }
        return null;
    }, Event.DEFAULT_PHASE, VAMPIRE_BLOOD_BACKED, DEFAULT_HEALTH_BACKED);

    @Nullable Storage<FluidVariant> findStorage(Entity entity);
}
