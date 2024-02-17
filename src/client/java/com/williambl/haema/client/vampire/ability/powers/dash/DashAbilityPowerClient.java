package com.williambl.haema.client.vampire.ability.powers.dash;

import com.williambl.haema.Haema;
import com.williambl.haema.client.vampire.ability.powers.VampireAbilityPowerTickKeybindsCallback;
import com.williambl.haema.vampire.HaemaVampires;
import com.williambl.haema.vampire.ability.powers.dash.DashAbilityPower;
import com.williambl.haema.vampire.ability.powers.dash.DashPacketC2S;
import com.williambl.haema.vampire.ability.powers.dash.EntityChargingDashComponent;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.minecraft.world.entity.Entity;

import java.util.*;

public class DashAbilityPowerClient {
    static int pressedTicks = 0;
    static int ticksSinceDashed = Integer.MAX_VALUE;

    public static final int DASH_FLASH_TICKS = 4;
    public static Map<Entity, Integer> dashedEntities = new LinkedHashMap<>();

    public static void init() {
        VampireAbilityPowerTickKeybindsCallback.event(DashAbilityPower.class).register((power, entity, source, active) -> {
            if (!active) {
                pressedTicks = -1;
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

        ClientPlayNetworking.registerGlobalReceiver(HaemaVampires.VampirePackets.DASH_FX, (packet, player, responseSender) -> {
            var e = player.level().getEntity(packet.entityId());
            if (e != null) {
                dashedEntities.put(e, DASH_FLASH_TICKS);
            }
        });

        ClientTickEvents.END_CLIENT_TICK.register($ -> List.copyOf(dashedEntities.keySet()).forEach(e -> dashedEntities.computeIfPresent(e, (k, v) -> v <= 1 ? null : v - 1)));

        DashShimmerEffectManager.INSTANCE.init();
        DashWobbleFx.init();
    }
}
