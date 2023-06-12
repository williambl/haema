package com.williambl.haema.api.content.blood;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;

//TODO maybe just use fluid transfer api ?
@FunctionalInterface
public interface EntityBloodExtractionCallback {
    ResourceLocation VAMPIRE_REMOVE_BLOOD = new ResourceLocation("haema:vampire_remove_blood");
    ResourceLocation DEFAULT_TAKE_DAMAGE = new ResourceLocation("haema:default_take_damage");

    Event<EntityBloodExtractionCallback> EVENT = EventFactory.createWithPhases(EntityBloodExtractionCallback.class, callbacks -> (e, quality, amount) -> {
        for (EntityBloodExtractionCallback callback : callbacks) {
            var result = callback.extractBlood(e, quality, amount);
            if (result) {
                return true;
            }
        }
        return false;
    }, Event.DEFAULT_PHASE, VAMPIRE_REMOVE_BLOOD, DEFAULT_TAKE_DAMAGE);

    boolean extractBlood(Entity entity, BloodQuality quality, long amount);
}
