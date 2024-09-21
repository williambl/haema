package com.williambl.haema;

import net.fabricmc.fabric.api.networking.v1.FabricPacket;

public interface HaemaClientHandler {
    boolean isKeybindPressed(String keybind);
    boolean consumeKeybind(String keybind);
    void send(FabricPacket packet);

    HaemaClientHandler DUMMY = new HaemaClientHandler() {
        @Override
        public boolean isKeybindPressed(String keybind) {
            return false;
        }

        @Override
        public boolean consumeKeybind(String keybind) {
            return false;
        }

        @Override
        public void send(FabricPacket packet) {}
    };
}
