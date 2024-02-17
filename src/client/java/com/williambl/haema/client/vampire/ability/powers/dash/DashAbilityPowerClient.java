package com.williambl.haema.client.vampire.ability.powers.dash;

import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.ability.powers.drinking.EntityDrinkTargetCallback;
import com.williambl.haema.client.vampire.ability.powers.VampireAbilityPowerTickKeybindsCallback;
import com.williambl.haema.vampire.ability.powers.dash.DashAbilityPower;
import com.williambl.haema.vampire.ability.powers.dash.DashPacketC2S;
import com.williambl.haema.vampire.ability.powers.dash.EntityChargingDashComponent;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingPacket;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;

public class DashAbilityPowerClient {
    static int pressedTicks = 0;
    static int ticksSinceDashed = Integer.MAX_VALUE;

    public static void init() {
        VampireAbilityPowerTickKeybindsCallback.event(DashAbilityPower.class).register((power, entity, source, active) -> {
            if (!active) {
                return;
            }
            var handler = Haema.CLIENT_HANDLER;
            boolean areAllPressed = power.keybinds().stream().allMatch(handler::isKeybindPressed);
            if (areAllPressed) {
                EntityChargingDashComponent.KEY.maybeGet(entity).ifPresent(c -> c.setChargingDash(true));
                ticksSinceDashed = -1;
                pressedTicks++;
            } else {
                EntityChargingDashComponent.KEY.maybeGet(entity).ifPresent(c -> c.setChargingDash(false));
                if (pressedTicks > 0) {
                    pressedTicks = 0;
                    ticksSinceDashed = 0;
                    handler.send(new DashPacketC2S());
                } else {
                    ticksSinceDashed++;
                }
            }
        });

        DashShimmerEffectManager.INSTANCE.init();
        DashWobbleFx.init();
    }
}
