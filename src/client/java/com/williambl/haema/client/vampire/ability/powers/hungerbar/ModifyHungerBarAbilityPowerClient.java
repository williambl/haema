package com.williambl.haema.client.vampire.ability.powers.hungerbar;

import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.hungerbar.ModifyHungerBarAbilityPower;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import org.jetbrains.annotations.Nullable;

public class ModifyHungerBarAbilityPowerClient {
    private static @Nullable ModifyHungerBarAbilityPower powerCache = null;
    private static double fullnessCache = 0.0;

    public static @Nullable ModifyHungerBarAbilityPower activePower() {
        return powerCache;
    }

    public static double fullnessValue() {
        return fullnessCache;
    }

    public static void init() {
        ClientTickEvents.START_CLIENT_TICK.register(client -> {
            if (client.player != null) {
                powerCache = client.player.getComponent(VampireAbilitiesComponent.KEY)
                        .getEnabledPowersOfClass(ModifyHungerBarAbilityPower.class)
                        .stream()
                        .findFirst()
                        .orElse(null);
                if (powerCache != null) {
                    fullnessCache = DFunctions.evaluate(powerCache.fullnessFunc(), DFunctions.createEntityContext(client.player));
                }
            }
        });
    }
}
