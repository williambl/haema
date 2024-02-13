package com.williambl.haema.vampire.ability.powers.drinking;

import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.HaemaVampires;
import net.fabricmc.fabric.api.networking.v1.FabricPacket;
import net.fabricmc.fabric.api.networking.v1.PacketType;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.entity.Entity;

public record DrinkingPacket(int target) implements FabricPacket {
    public DrinkingPacket(FriendlyByteBuf buf) {
        this(buf.readVarInt());
    }

    public DrinkingPacket(Entity target) {
        this(target.getId());
    }

    @Override
    public void write(FriendlyByteBuf buf) {
        buf.writeVarInt(this.target());
    }

    @Override
    public PacketType<?> getType() {
        return HaemaVampires.VampirePackets.DRINKING;
    }

    public static void init() {
        ServerPlayNetworking.registerGlobalReceiver(HaemaVampires.VampirePackets.DRINKING, (packet, player, responseSender) -> {
            var target = player.level().getEntity(packet.target());
            if (target != null) {
                // findFirst short circuits, but complains if we don't put it into a variable lol
                var ignored = VampireAbilitiesComponent.KEY.maybeGet(player).stream()
                        .flatMap(c -> c.getEnabledPowersOfClass(DrinkingAbilityPower.class).stream())
                        .filter(p -> p.tryDrink(player, target))
                        .findFirst();
            }
        });
    }
}
