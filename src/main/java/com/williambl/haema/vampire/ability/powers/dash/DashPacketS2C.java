package com.williambl.haema.vampire.ability.powers.dash;

import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.HaemaVampires;
import net.fabricmc.fabric.api.networking.v1.FabricPacket;
import net.fabricmc.fabric.api.networking.v1.PacketType;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;

public record DashPacketS2C(int entityId) implements FabricPacket {
    public DashPacketS2C(FriendlyByteBuf buf) {
        this(buf.readVarInt());
    }

    @Override
    public void write(FriendlyByteBuf buf) {
        buf.writeVarInt(this.entityId());
    }

    @Override
    public PacketType<?> getType() {
        return HaemaVampires.VampirePackets.DASH_FX;
    }
}
