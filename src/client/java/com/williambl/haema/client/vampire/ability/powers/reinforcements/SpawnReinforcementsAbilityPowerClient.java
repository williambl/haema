package com.williambl.haema.client.vampire.ability.powers.reinforcements;

import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.ability.powers.drinking.EntityDrinkTargetCallback;
import com.williambl.haema.client.vampire.ability.powers.VampireAbilityPowerTickKeybindsCallback;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingPacket;
import com.williambl.haema.vampire.ability.powers.reinforcements.SpawnReinforcementsAbilityPower;
import com.williambl.haema.vampire.ability.powers.reinforcements.SpawnReinforcementsPacket;

public class SpawnReinforcementsAbilityPowerClient {
    public static void init() {
        VampireAbilityPowerTickKeybindsCallback.event(SpawnReinforcementsAbilityPower.class).register((power, entity, source, active) -> {
            var handler = Haema.CLIENT_HANDLER;
            if (active && power.keybinds().stream().allMatch(handler::isKeybindPressed)) {
                power.keybinds().forEach(handler::consumeKeybind);
                handler.send(new SpawnReinforcementsPacket());
            }
        });
    }
}
