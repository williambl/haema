package com.williambl.haema.ritual.altar;

import net.fabricmc.api.EnvType;
import net.fabricmc.api.Environment;
import net.fabricmc.fabric.api.networking.v1.PacketByteBufs;
import net.fabricmc.fabric.api.networking.v1.PlayerLookup;
import net.fabricmc.fabric.api.networking.v1.ServerPlayNetworking;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.block.entity.BlockEntity;

import static com.williambl.haema.Haema.id;

public interface SyncedBlockEntity {
    ResourceLocation PACKET_ID = id("block_entity_sync");

    void writeSyncPacket(FriendlyByteBuf buf, ServerPlayer recipient);
    @Environment(EnvType.CLIENT)
    void applySyncPacket(FriendlyByteBuf buf);

    default void sync() {
        if (this instanceof BlockEntity blockEntity) {
            PlayerLookup.tracking(blockEntity).forEach(player -> {
                FriendlyByteBuf buf = PacketByteBufs.create();
                buf.writeBlockPos(blockEntity.getBlockPos());
                writeSyncPacket(buf, player);
                ServerPlayNetworking.send(player, PACKET_ID, buf);
            });
        }
    }
}
