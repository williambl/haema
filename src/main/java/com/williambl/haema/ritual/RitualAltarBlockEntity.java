package com.williambl.haema.ritual;

import com.williambl.haema.api.ritual.RitualArae;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.pattern.BlockInWorld;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;
import java.util.Optional;

public class RitualAltarBlockEntity extends BlockEntity {
    private @Nullable ResourceKey<RitualArae> arae;

    public RitualAltarBlockEntity(BlockPos blockPos, BlockState blockState) {
        super(HaemaRituals.RitualBlockEntities.RITUAL_ALTAR, blockPos, blockState);
    }

    // naive implementation
    // TODO cache araes, cache predicates, cache blockinworld even? or somehow check these in parallel. also invalidate cache on datapack reload.
    public Optional<RitualArae> checkArae() {
        if (this.level == null) {
            return Optional.empty();
        }
        var registry = this.level.registryAccess().registryOrThrow(RitualArae.REGISTRY_KEY);
        return registry.stream()
                .filter(ritualArae -> ritualArae.multiblock().createPredicate().test(new BlockInWorld(this.level, this.getBlockPos(), true)))
                .findFirst();
    }

    public static void tick(Level level, BlockPos blockPos, BlockState blockState, RitualAltarBlockEntity entity) {
        var registry = level.registryAccess().registryOrThrow(RitualArae.REGISTRY_KEY);
        if (entity.arae == null) {
            entity.arae = entity.checkArae().flatMap(registry::getResourceKey).orElse(null);
        }

        if (entity.arae != null) {
            var arae = Objects.requireNonNull(registry.get(entity.arae));
            if (!arae.multiblock().createPredicate().test(new BlockInWorld(level, blockPos, true))) {
                entity.arae = null;
            }

            arae.modules().forEach(module -> module.onAraeTick(arae, level, blockPos));
            System.out.printf("tick %s%n", entity.arae);
        }
    }
}
