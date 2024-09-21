package com.williambl.haema.api.vampire.ability.powers.drinking;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

import java.util.Optional;

@FunctionalInterface
public interface EntityDrinkTargetCallback {
    Event<EntityDrinkTargetCallback> EVENT = EventFactory.createArrayBacked(EntityDrinkTargetCallback.class, callbacks -> v -> {
        for (var callback : callbacks) {
            var res = callback.getTarget(v);
            if (res.isPresent()) {
                return res;
            }
        }

        return Optional.empty();
    });

    Optional<Entity> getTarget(LivingEntity vampire);
}
