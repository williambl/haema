package com.williambl.haema.ritual;

import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.ritual.altar.ChunkChangeProvider;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.minecraft.core.BlockPos;
import net.minecraft.core.RegistryAccess;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.pattern.BlockInWorld;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

public class RitualAltarBlockEntity extends BlockEntity {
    private static final String ARAE_TAG_KEY = "arae";
    private static List<AraeCacheEntry> araeCache;
    private static Map<ResourceKey<RitualArae>, AraeCacheEntry> araeCacheByKey;
    private static int overallMinXFocusSpace;
    private static int overallMinYFocusSpace;
    private static int overallMinZFocusSpace;
    private static int overallMaxXFocusSpace;
    private static int overallMaxYFocusSpace;
    private static int overallMaxZFocusSpace;

    private @Nullable ResourceKey<RitualArae> arae;
    private long lastCheckTime = Long.MIN_VALUE;

    public RitualAltarBlockEntity(BlockPos blockPos, BlockState blockState) {
        super(HaemaRituals.RitualBlockEntities.RITUAL_ALTAR, blockPos, blockState);
    }

    @Override
    protected void saveAdditional(@NotNull CompoundTag compoundTag) {
        super.saveAdditional(compoundTag);
        if (arae != null) {
            compoundTag.putString(ARAE_TAG_KEY, arae.location().toString());
        }
    }

    @Override
    public void load(@NotNull CompoundTag compoundTag) {
        super.load(compoundTag);
        if (compoundTag.contains(ARAE_TAG_KEY)) {
            String location = compoundTag.getString(ARAE_TAG_KEY);
            if (ResourceLocation.isValidResourceLocation(location)) {
                arae = ResourceKey.create(RitualArae.REGISTRY_KEY, new ResourceLocation(location));
            }
        }
    }

    // TODO skip over if a large arae gets removed & searchspace is shrunk.
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
            rebuildCache(level.registryAccess());
        }

        level.getProfiler().push("ritual_altar_tick");
        int chunkX = blockPos.getX() >> 4;
        int chunkZ = blockPos.getZ() >> 4;
        boolean shouldCheck = false;
        for (int dX = -1; dX < 2; dX++) {
            for (int dZ = -1; dZ < 2; dZ++) {
                shouldCheck |= ((ChunkChangeProvider) level).lastChunkChange(chunkX + dX, chunkZ + dZ) > entity.lastCheckTime;
            }
        }

        if (shouldCheck) {
            entity.lastCheckTime = level.getGameTime();
            if (entity.arae == null || !(araeCacheByKey.containsKey(entity.arae))) {
                entity.arae = entity.checkArae(new ArrayList<>(araeCache)).map(AraeCacheEntry::key).orElse(null);
            } else {
                if (entity.checkArae(List.of(araeCacheByKey.get(entity.arae))).isEmpty()) {
                    entity.arae = null;
                }
            }

            if (entity.arae != null) {
                var arae = araeCacheByKey.get(entity.arae);
                arae.arae().modules().forEach(module -> module.onAraeTick(arae.arae(), level, blockPos));
            }
        }

        level.getProfiler().pop();
    }

    public static void initReloadListener() {
        ServerLifecycleEvents.END_DATA_PACK_RELOAD.register((server, resourceManager, success) -> {
            rebuildCache(server.registryAccess());
        });
    }

    private static void rebuildCache(RegistryAccess registryAccess) {
        var registry = registryAccess.registryOrThrow(RitualArae.REGISTRY_KEY);
        araeCache = registry.holders()
                .map(ref -> AraeCacheEntry.create(
                        ref.key(),
                        ref.value()))
                .toList();
        araeCacheByKey = araeCache.stream().collect(Collectors.toMap(AraeCacheEntry::key, Function.identity()));
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
