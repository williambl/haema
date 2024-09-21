package com.williambl.haema.mixin.ritual.altar;

import com.williambl.haema.ritual.altar.ChunkChangeProvider;
import it.unimi.dsi.fastutil.longs.Long2LongMap;
import it.unimi.dsi.fastutil.longs.Long2LongOpenHashMap;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.profiling.ProfilerFiller;
import net.minecraft.world.level.ChunkPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.dimension.DimensionType;
import net.minecraft.world.level.storage.WritableLevelData;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import java.util.function.Supplier;

@Mixin(ServerLevel.class)
public abstract class ServerLevelMixin extends Level implements ChunkChangeProvider {
    protected ServerLevelMixin(WritableLevelData writableLevelData, ResourceKey<Level> resourceKey, RegistryAccess registryAccess, Holder<DimensionType> holder, Supplier<ProfilerFiller> supplier, boolean bl, boolean bl2, long l, int i) {
        super(writableLevelData, resourceKey, registryAccess, holder, supplier, bl, bl2, l, i);
    }

    private final @Unique Long2LongMap chunkUpdateTimes = new Long2LongOpenHashMap();

    @Inject(method = "blockUpdated", at = @At("HEAD"))
    private void haema$setChunkUpdateTime(BlockPos blockPos, Block block, CallbackInfo ci) {
        this.chunkUpdateTimes.put(ChunkPos.asLong(blockPos), this.getGameTime());
    }

    @Override
    public long lastChunkChange(int chunkX, int chunkZ) {
        return this.chunkUpdateTimes.getOrDefault(ChunkPos.asLong(chunkX, chunkZ), 0);
    }
}
