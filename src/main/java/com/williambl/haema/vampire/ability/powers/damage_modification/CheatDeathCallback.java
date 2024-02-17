package com.williambl.haema.vampire.ability.powers.damage_modification;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.LivingEntity;

@FunctionalInterface
public interface CheatDeathCallback {
    Event<CheatDeathCallback> EVENT = EventFactory.createArrayBacked(CheatDeathCallback.class, (listeners) -> (source, amount, entity) -> {
        for (var listener : listeners) {
            if (listener.shouldSurvive(source, amount, entity)) {
                return true;
            }
        }
        return false;
    });

    boolean shouldSurvive(DamageSource source, float amount, LivingEntity entity);
}
