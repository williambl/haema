package com.williambl.haema.ritual;

import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.api.ritual.RitualArae;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.pattern.BlockInWorld;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class RitualAltarBlockEntity extends BlockEntity {
    private static List<AraeCacheEntry> araeCache;
    private static int overallMinXFocusSpace;
    private static int overallMinYFocusSpace;
    private static int overallMinZFocusSpace;
    private static int overallMaxXFocusSpace;
    private static int overallMaxYFocusSpace;
    private static int overallMaxZFocusSpace;

    private @Nullable AraeCacheEntry arae;

    public RitualAltarBlockEntity(BlockPos blockPos, BlockState blockState) {
        super(HaemaRituals.RitualBlockEntities.RITUAL_ALTAR, blockPos, blockState);
    }

    // TODO skip over if a large arae gets removed & searchspace is shrunk. also invalidate cache on datapack reload.
    public Optional<AraeCacheEntry> checkArae(List<AraeCacheEntry> possibleAraes) {
        if (this.level == null) {
            return Optional.empty();
        }

        BlockPos focusPosWorldSpace = this.getBlockPos();
        BlockPos.MutableBlockPos posWorldSpace = this.getBlockPos().mutable();
        for (int yFocusSpace = overallMinYFocusSpace; yFocusSpace <= overallMaxYFocusSpace; yFocusSpace++) {
            for (int zFocusSpace = overallMinZFocusSpace; zFocusSpace <= overallMaxZFocusSpace; zFocusSpace++) {
                for (int xFocusSpace = overallMinXFocusSpace; xFocusSpace <= overallMaxXFocusSpace; xFocusSpace++) {
                    posWorldSpace.set(focusPosWorldSpace);
                    posWorldSpace.move(xFocusSpace, yFocusSpace, zFocusSpace);
                    var blockInWorld = new BlockInWorld(this.level, posWorldSpace, true);
                    var dfcontext = HaemaDFunctions.blockInWorldFast(blockInWorld);
                    for (var iter = possibleAraes.iterator(); iter.hasNext();) {
                        var arae = iter.next();
                        if (!arae.arae().multiblock().matches(xFocusSpace, yFocusSpace, zFocusSpace, dfcontext)) {
                            iter.remove();
                            if (possibleAraes.isEmpty()) {
                                return Optional.empty();
                            }
                        }
                    }
                }
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
            entity.arae = entity.checkArae(new ArrayList<>(araeCache)).orElse(null);
        }

        if (entity.arae != null) {
            if (entity.checkArae(List.of(entity.arae)).isEmpty()) {
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
                .map(ref -> AraeCacheEntry.create(
                        ref.key(),
                        ref.value()))
                .toList();
        overallMinXFocusSpace = araeCache.stream().mapToInt(AraeCacheEntry::minXFocusSpace).min().orElse(0);
        overallMinYFocusSpace = araeCache.stream().mapToInt(AraeCacheEntry::minYFocusSpace).min().orElse(0);
        overallMinZFocusSpace = araeCache.stream().mapToInt(AraeCacheEntry::minZFocusSpace).min().orElse(0);
        overallMaxXFocusSpace = araeCache.stream().mapToInt(AraeCacheEntry::maxXFocusSpace).max().orElse(0);
        overallMaxYFocusSpace = araeCache.stream().mapToInt(AraeCacheEntry::maxYFocusSpace).max().orElse(0);
        overallMaxZFocusSpace = araeCache.stream().mapToInt(AraeCacheEntry::maxZFocusSpace).max().orElse(0);
    }

    private record AraeCacheEntry(ResourceKey<RitualArae> key, RitualArae arae, int minXFocusSpace, int minYFocusSpace, int minZFocusSpace, int maxXFocusSpace, int maxYFocusSpace, int maxZFocusSpace) {
        private static AraeCacheEntry create(ResourceKey<RitualArae> key, RitualArae arae) {
            var pattern = arae.multiblock().pattern();
            var focusPos = arae.multiblock().focusPosFilterSpace();
            int patternYSize = pattern.length;
            int patternZSize = pattern[0].length;
            int patternXSize = pattern[0][0].length; // we know that the pattern is cuboid
            return new AraeCacheEntry(key, arae,
                    -focusPos.getX(),
                    -focusPos.getY(),
                    -focusPos.getZ(),
                    patternXSize - focusPos.getX() - 1,
                    patternYSize - focusPos.getY() - 1,
                    patternZSize - focusPos.getZ() - 1);
        }
    }
}
