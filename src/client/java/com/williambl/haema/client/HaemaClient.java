package com.williambl.haema.client;

import com.williambl.haema.Haema;
import com.williambl.haema.HaemaClientHandler;
import com.williambl.haema.client.client.content.HaemaContentClient;
import com.williambl.haema.client.hunters.HaemaHuntersClient;
import com.williambl.haema.client.mixin.KeyMappingAccessor;
import com.williambl.haema.client.ritual.HaemaRitualsClient;
import com.williambl.haema.client.vampire.HaemaVampiresClient;
import com.williambl.haema.client.vampire_mobs.HaemaVampireMobsClient;
import net.fabricmc.api.ClientModInitializer;
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.fabricmc.fabric.api.networking.v1.FabricPacket;
import net.minecraft.client.KeyMapping;
import net.minecraft.client.Minecraft;

import java.util.Optional;

public class HaemaClient implements ClientModInitializer {
    @Override
    public void onInitializeClient() {
        HaemaVampiresClient.init();
        HaemaContentClient.init();
        HaemaRitualsClient.init();
        HaemaHuntersClient.init();
        HaemaVampireMobsClient.init();
        Haema.CLIENT_HANDLER = new HaemaClientHandler() {
            @Override
            public boolean isKeybindPressed(String keybind) {
                return Optional.ofNullable(KeyMappingAccessor.getALL().get(keybind)).filter(KeyMapping::isDown).isPresent();
            }

            @Override
            public boolean consumeKeybind(String keybind) {
                return Optional.ofNullable(KeyMappingAccessor.getALL().get(keybind)).filter(KeyMapping::consumeClick).isPresent();
            }

            @Override
            public void send(FabricPacket packet) {
                ClientPlayNetworking.send(packet);
            }
        };
    }
}
