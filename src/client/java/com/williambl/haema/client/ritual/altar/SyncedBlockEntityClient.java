package com.williambl.haema.client.ritual.altar;

import com.williambl.haema.Haema;
import com.williambl.haema.ritual.altar.SyncedBlockEntity;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;

public class SyncedBlockEntityClient {
    public static void init() {
        ClientPlayNetworking.registerGlobalReceiver(SyncedBlockEntity.PACKET_ID, (client, handler, buf, responseSender) -> {
            BlockPos pos = buf.readBlockPos();
            FriendlyByteBuf bufCopy = new FriendlyByteBuf(buf.copy());
            client.execute(() -> {
                try {
                    if (client.level == null) {
                        return;
                    }

                    var entity = client.level.getBlockEntity(pos);
                    if (entity instanceof SyncedBlockEntity syncedBlockEntity) {
                        syncedBlockEntity.applySyncPacket(bufCopy);
                    } else {
                        Haema.LOGGER.warn("Received block entity sync packet for position with invalid blcck entity: {}", pos);
                    }
                } finally {
                    bufCopy.release();
                }
            });
        });
    }
}
