package com.williambl.haema.vampire.ability.powers.dash;

import com.mojang.brigadier.Command;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.HaemaVampires;
import net.fabricmc.fabric.api.networking.v1.FabricPacket;
import net.fabricmc.fabric.api.networking.v1.PacketType;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;

public record DashPacketC2S() implements FabricPacket {
    public DashPacketC2S(FriendlyByteBuf buf) {
        this();
    }

    @Override
    public void write(FriendlyByteBuf buf) {

    }

    @Override
    public PacketType<?> getType() {
        return HaemaVampires.VampirePackets.DASH;
    }

    public static void init() {
        ServerPlayNetworking.registerGlobalReceiver(HaemaVampires.VampirePackets.DASH, (packet, player, responseSender) -> {
            var power = VampireAbilitiesComponent.KEY.get(player).getEnabledPowersOfClass(DashAbilityPower.class).stream().findFirst();
            power.ifPresent(dashAbilityPower -> dashAbilityPower.dash(player));
        });
    }
}
