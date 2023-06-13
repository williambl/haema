package com.williambl.haema.mixin.ritual.altar;

import com.williambl.haema.ritual.altar.SyncedBlockEntity;
import net.minecraft.network.protocol.game.ClientboundLevelChunkWithLightPacket;
import net.minecraft.server.level.ChunkMap;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.chunk.LevelChunk;
import org.apache.commons.lang3.mutable.MutableObject;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ChunkMap.class)
public class ChunkMapMixin {
    @Inject(method = "playerLoadedChunk", at = @At("RETURN"))
    private void haema$syncBlockEntities(ServerPlayer serverPlayer, MutableObject<ClientboundLevelChunkWithLightPacket> mutableObject, LevelChunk levelChunk, CallbackInfo ci) {
        for (var be : levelChunk.getBlockEntities().values()) {
            if (be instanceof SyncedBlockEntity synced) {
                synced.sync();
            }
        }
    }
}
