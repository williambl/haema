package com.williambl.haema.api.vampire.ability.powers.drinking;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

import java.util.Optional;

@FunctionalInterface
public interface OnVampireDrinkCallback {
    Event<OnVampireDrinkCallback> EVENT = EventFactory.createArrayBacked(OnVampireDrinkCallback.class, callbacks -> (vampire, target) -> {
        for (var callback : callbacks) {
            callback.onDrink(vampire, target);
        }
    });

    void onDrink(LivingEntity vampire, Entity target);
}
