package com.williambl.haema.client.vampire.ability.powers;

import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.fabricmc.fabric.api.event.Event;
import net.fabricmc.fabric.api.event.EventFactory;
import net.minecraft.client.player.LocalPlayer;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@FunctionalInterface
public interface VampireAbilityPowerTickKeybindsCallback<T extends VampireAbilityPower> {
    Map<Class<? extends VampireAbilityPower>, Event<? extends VampireAbilityPowerTickKeybindsCallback<?>>> EVENTS = new HashMap<>();

    static <T extends VampireAbilityPower> Event<VampireAbilityPowerTickKeybindsCallback<T>> event(Class<T> powerClass) {
        @SuppressWarnings("unchecked") Event<VampireAbilityPowerTickKeybindsCallback<T>> event
                = (Event<VampireAbilityPowerTickKeybindsCallback<T>>) EVENTS.computeIfAbsent(powerClass,
                $ -> EventFactory.<VampireAbilityPowerTickKeybindsCallback<T>>createArrayBacked(VampireAbilityPowerTickKeybindsCallback.class,
                        callbacks -> (p, e, s) -> {
                            for (var c : callbacks) {
                                c.tickKeybinds(powerClass.cast(p), e, s);
                            }
                        }));
        return event;
    }

    static void invokeAll(Set<VampireAbility> abilities, LocalPlayer player) {
        for (var ability : abilities) {
            for (var power : ability.powers()) {
                @SuppressWarnings("unchecked")
                Event<VampireAbilityPowerTickKeybindsCallback<VampireAbilityPower>> event =
                        (Event<VampireAbilityPowerTickKeybindsCallback<VampireAbilityPower>>) EVENTS.get(power.getClass());
                if (event != null) {
                    event.invoker().tickKeybinds(power, player, ability);
                }
            }
        }
    }

    void tickKeybinds(T power, LocalPlayer entity, VampireAbility source);
}
