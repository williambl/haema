package com.williambl.haema.api.vampire.ability.powers.sunlight_sickness;

import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;

import static com.williambl.haema.Haema.id;

public final class VampireBurnEvents {
    public static final Event<TriggerVampireBurn> TRIGGER = EventFactory.createArrayBacked(TriggerVampireBurn.class, (listeners) -> (vampire) -> {
        for (TriggerVampireBurn listener : listeners) {
            if (listener.shouldTriggerVampireBurn(vampire)) {
                return true;
            }
        }
        return false;
    });

    public static final ResourceLocation PREVENT_PHASE_ARMOUR = id("armour"); //TODO use this

    public static final Event<PreventVampireBurn> PREVENT = EventFactory.createWithPhases(PreventVampireBurn.class, (listeners) -> (vampire) -> {
        for (PreventVampireBurn listener : listeners) {
            if (listener.shouldPreventVampireBurn(vampire)) {
                return true;
            }
        }
        return false;
    }, Event.DEFAULT_PHASE, PREVENT_PHASE_ARMOUR);

    @FunctionalInterface
    public interface TriggerVampireBurn {
        boolean shouldTriggerVampireBurn(LivingEntity vampire);
    }

    @FunctionalInterface
    public interface PreventVampireBurn {
        boolean shouldPreventVampireBurn(LivingEntity vampire);
    }
}
