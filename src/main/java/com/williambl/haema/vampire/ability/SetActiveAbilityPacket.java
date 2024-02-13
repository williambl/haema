package com.williambl.haema.vampire.ability;

import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.VampireApi;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.vampire.HaemaVampires;
import net.fabricmc.fabric.api.networking.v1.FabricPacket;
import net.fabricmc.fabric.api.networking.v1.PacketType;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceKey;

import java.util.Optional;

public record SetActiveAbilityPacket(Optional<ResourceKey<VampireAbility>> key) implements FabricPacket {
    public SetActiveAbilityPacket(FriendlyByteBuf buf) {
        this(buf.readOptional(b -> b.readResourceKey(VampireAbility.REGISTRY_KEY)));
    }

    @Override
    public void write(FriendlyByteBuf buf) {
        buf.writeOptional(this.key(), FriendlyByteBuf::writeResourceKey);
    }

    @Override
    public PacketType<?> getType() {
        return HaemaVampires.VampirePackets.SET_ACTIVE_ABILITY;
    }

    public static void init() {
        ServerPlayNetworking.registerGlobalReceiver(HaemaVampires.VampirePackets.SET_ACTIVE_ABILITY, (packet, player, responseSender) -> {
            var component = VampireAbilitiesComponent.KEY.get(player);
            var registry = player.level().registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY);
            if (packet.key().isPresent()) {
                var ability = registry.getOptional(packet.key().get());
                if (ability.isPresent()) {
                    if (!component.setActiveAbility(ability.get())) {
                        Haema.LOGGER.error("Could not set active ability {} on player {}", packet.key().get().location(), player.getScoreboardName());
                    }
                } else {
                    Haema.LOGGER.error("Processing packet to set active ability for player {}, could not find ability {}", player.getScoreboardName(), packet.key().get().location());
                }
            } else {
                component.setActiveAbility(null);
            }
        });
    }
}
