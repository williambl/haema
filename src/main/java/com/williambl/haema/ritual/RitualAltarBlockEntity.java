package com.williambl.haema.ritual;

import com.williambl.haema.api.ritual.RitualArae;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Holder;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.pattern.BlockInWorld;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class RitualAltarBlockEntity extends BlockEntity {
    private static Map<ResourceKey<RitualArae>, AraeCacheEntry> araeCache;
    private static List<AraeCacheEntry> araeCacheList;
    private @Nullable AraeCacheEntry arae;

    public RitualAltarBlockEntity(BlockPos blockPos, BlockState blockState) {
        super(HaemaRituals.RitualBlockEntities.RITUAL_ALTAR, blockPos, blockState);
    }

    // naive implementation
    // TODO cache blockinworld? or somehow check these in parallel. also invalidate cache on datapack reload.
    public Optional<AraeCacheEntry> checkArae() {
        if (this.level == null) {
            return Optional.empty();
        }

        for (var ritualArae : araeCacheList) {
            if (ritualArae.predicate().test(new BlockInWorld(this.level, this.getBlockPos(), true))) {
                return Optional.of(ritualArae);
            }
        }

        return Optional.empty();
    }

    public static void tick(Level level, BlockPos blockPos, BlockState blockState, RitualAltarBlockEntity entity) {
        if (!(level instanceof ServerLevel)) {
            return;
        }

        if (araeCache == null) {
            rebuildCache(level);
        }

        level.getProfiler().push("ritual_altar_tick");
        if (entity.arae == null) {
            entity.arae = entity.checkArae().orElse(null);
        }

        if (entity.arae != null) {
            if (!entity.arae.predicate().test(new BlockInWorld(level, blockPos, true))) {
                entity.arae = null;
            }

            entity.arae.arae().modules().forEach(module -> module.onAraeTick(entity.arae.arae(), level, blockPos));
        }
        level.getProfiler().pop();
    }

    //TODO also replace the arae field on BE instances
    //TODO do this on datapack reload
    private static void rebuildCache(Level level) {
        var registry = level.registryAccess().registryOrThrow(RitualArae.REGISTRY_KEY);
        araeCache = registry.holders()
                .collect(Collectors.toMap(
                        Holder.Reference::key,
                        ref -> new AraeCacheEntry(ref.key(), ref.value(), ref.value().multiblock().createPredicate())));
        araeCacheList = new ArrayList<>(araeCache.values());
    }

    private record AraeCacheEntry(ResourceKey<RitualArae> key, RitualArae arae, Predicate<BlockInWorld> predicate) {}
}
