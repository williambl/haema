package com.williambl.haema.vampire.ability.powers.dash;

import dev.onyxstudios.cca.api.v3.component.Component;
import dev.onyxstudios.cca.api.v3.component.ComponentKey;
import dev.onyxstudios.cca.api.v3.component.ComponentRegistry;
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;

import static com.williambl.haema.Haema.id;

public class EntityChargingDashComponent implements Component, AutoSyncedComponent {
    private boolean isChargingDash = true;

    @Override
    public void readFromNbt(CompoundTag tag) {
        // do not persist
    }

    @Override
    public void writeToNbt(CompoundTag tag) {
        // do not persist
    }

    @Override
    public void writeSyncPacket(FriendlyByteBuf buf, ServerPlayer recipient) {
        buf.writeBoolean(this.isChargingDash);
    }

    @Override
    public void applySyncPacket(FriendlyByteBuf buf) {
        this.isChargingDash = buf.readBoolean();
    }

    public boolean isChargingDash() {
        return this.isChargingDash;
    }

    public void setChargingDash(boolean chargingDash) {
        this.isChargingDash = chargingDash;
    }

    public static final ComponentKey<EntityChargingDashComponent> KEY = ComponentRegistry.getOrCreate(id("charging_dash"), EntityChargingDashComponent.class);
}
