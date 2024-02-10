package com.williambl.haema.client.vampire.ability.powers.drinking;

import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.haema.api.vampire.ability.powers.drinking.EntityDrinkTargetCallback;
import com.williambl.haema.client.vampire.ability.powers.VampireAbilityPowerTickKeybindsCallback;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingPacket;
import net.minecraft.world.entity.player.Player;

public class DrinkingAbilityPowerClient {
    public static void init() {
        VampireAbilityPowerTickKeybindsCallback.event(DrinkingAbilityPower.class).register((power, entity, source) -> {
            var handler = Haema.CLIENT_HANDLER;
            if (power.keybinds().stream().allMatch(handler::isKeybindPressed)) {
                power.keybinds().forEach(handler::consumeKeybind);
                EntityDrinkTargetCallback.EVENT.invoker().getTarget(entity).ifPresent(target -> {
                    handler.send(new DrinkingPacket(target));
                });
            }
        });
    }
}
