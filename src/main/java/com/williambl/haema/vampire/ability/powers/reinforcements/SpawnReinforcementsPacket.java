package com.williambl.haema.vampire.ability.powers.reinforcements;

import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.HaemaVampires;
import com.williambl.haema.vampire.ability.powers.drinking.DrinkingAbilityPower;
import net.fabricmc.fabric.api.networking.v1.FabricPacket;
import net.fabricmc.fabric.api.networking.v1.PacketType;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.entity.Entity;

public record SpawnReinforcementsPacket() implements FabricPacket {
    public SpawnReinforcementsPacket(FriendlyByteBuf buf) {
        this();
    }

    @Override
    public void write(FriendlyByteBuf buf) {
    }

    @Override
    public PacketType<?> getType() {
        return HaemaVampires.VampirePackets.SPAWN_REINFORCEMENTS;
    }

    public static void init() {
        ServerPlayNetworking.registerGlobalReceiver(HaemaVampires.VampirePackets.SPAWN_REINFORCEMENTS, (packet, player, responseSender) -> {
            // findFirst short circuits, but complains if we don't put it into a variable lol
            var ignored = VampireAbilitiesComponent.KEY.maybeGet(player).stream()
                    .flatMap(c -> c.getEnabledPowersOfClass(SpawnReinforcementsAbilityPower.class).stream())
                    .filter(p -> p.trySpawnReinforcements(player, player.serverLevel()))
                    .findFirst();
        });
    }
}
