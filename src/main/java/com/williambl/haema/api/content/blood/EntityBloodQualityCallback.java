package com.williambl.haema.api.content.blood;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.entity.Entity;

import java.util.Optional;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
@FunctionalInterface
public interface EntityBloodQualityCallback {
    Event<EntityBloodQualityCallback> EVENT = EventFactory.createArrayBacked(EntityBloodQualityCallback.class, callbacks -> (e, original) -> {
        for (EntityBloodQualityCallback callback : callbacks) {
            var result = callback.getBloodQuality(e, original);
            if (result.getResult().consumesAction()) {
                return result;
            }
        }
        return InteractionResultHolder.pass(original);
    });

    InteractionResultHolder<Optional<BloodQuality>> getBloodQuality(Entity entity, Optional<BloodQuality> original);
}
