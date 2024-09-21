package com.williambl.haema.vampire.ability.powers.damage_modification;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.LivingEntity;

@FunctionalInterface
public interface DamageModificationCallback {
    Event<DamageModificationCallback> EVENT = EventFactory.createArrayBacked(DamageModificationCallback.class, (listeners) -> (source, amount, entity) -> {
        for (var listener : listeners) {
            amount = listener.modifyDamage(source, amount, entity);
        }
        return amount;
    });

    float modifyDamage(DamageSource source, float amount, LivingEntity entity);
}
