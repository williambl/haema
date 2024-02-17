package com.williambl.haema.mixin.ritual.altar;

import com.williambl.haema.ritual.altar.SyncedBlockEntity;
import net.minecraft.network.protocol.game.ClientboundLevelChunkWithLightPacket;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.network.PlayerChunkSender;
import net.minecraft.server.network.ServerGamePacketListenerImpl;
import net.minecraft.world.level.chunk.LevelChunk;
import org.apache.commons.lang3.mutable.MutableObject;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(PlayerChunkSender.class)
public class PlayerChunkSenderMixin {
    @Inject(method = "sendChunk", at = @At("RETURN"))
    private static void haema$syncBlockEntities(ServerGamePacketListenerImpl serverGamePacketListenerImpl, ServerLevel serverLevel, LevelChunk levelChunk, CallbackInfo ci) {
        for (var be : levelChunk.getBlockEntities().values()) {
            if (be instanceof SyncedBlockEntity synced) {
                synced.sync();
            }
        }
    }
}
