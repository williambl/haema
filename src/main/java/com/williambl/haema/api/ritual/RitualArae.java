package com.williambl.haema.api.ritual;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.module.AraeModule;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import org.jetbrains.annotations.Nullable;

import java.util.*;

import static com.williambl.haema.Haema.id;

/**
 * A ritual arae is a multiblock structure that can be used to perform rituals. These are defined in datapacks.
 * One instance of {@code RitualArae} represents one type of Arae, like how one instance of {@code Block} represents
 * one type of block.
 * @param multiblock    the multiblock filter for this arae
 * @param fluidSpaces   the characters in the multiblock template which represent spaces for fluid
 * @param modules       the modules for this arae
 */
public record RitualArae(MultiblockFilter multiblock, Set<Character> fluidSpaces, List<AraeModule> modules) {
    public static final ResourceKey<Registry<RitualArae>> REGISTRY_KEY = ResourceKey.createRegistryKey(id( "ritual_arae"));

    public static final Codec<RitualArae> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            MultiblockFilter.CODEC.fieldOf("multiblock").forGetter(RitualArae::multiblock),
            HaemaUtil.CHARACTER_CODEC.listOf().<Set<Character>>xmap(HashSet::new, ArrayList::new).fieldOf("fluid_spaces").forGetter(RitualArae::fluidSpaces),
            AraeModule.MODULE_CODEC.listOf().fieldOf("modules").forGetter(RitualArae::modules)
    ).apply(instance, RitualArae::new));

    public Iterable<BlockPos> getFluidSpaces(BlockPos focusPosWorldSpace) {
        return () -> new Iterator<BlockPos>() {
            final BlockPos initialPos = focusPosWorldSpace.subtract(RitualArae.this.multiblock().focusPosFilterSpace());
            final BlockPos.MutableBlockPos pos = this.initialPos.mutable();
            int currentYFilterSpace = 0;
            int currentZFilterSpace = 0;
            int currentXFilterSpace = -1;
            @Override
            public boolean hasNext() {
                return this.nextFilterSpacePos() != null;
            }

            @Override
            public BlockPos next() {
                var nextFilterSpace = this.nextFilterSpacePos();
                if (nextFilterSpace == null) {
                    throw new NoSuchElementException();
                }
                this.currentXFilterSpace = nextFilterSpace.getX();
                this.currentYFilterSpace = nextFilterSpace.getY();
                this.currentZFilterSpace = nextFilterSpace.getZ();
                return this.pos.set(this.initialPos).move(this.currentXFilterSpace, this.currentYFilterSpace, this.currentZFilterSpace);
            }

            @Nullable BlockPos nextFilterSpacePos() {
                int startZ = this.currentZFilterSpace;
                int startX = this.currentXFilterSpace+1;
                for (int yFilterSpace = this.currentYFilterSpace; yFilterSpace < RitualArae.this.multiblock.pattern().length; yFilterSpace++) {
                    for (int zFilterSpace = startZ; zFilterSpace < RitualArae.this.multiblock.pattern()[0].length; zFilterSpace++) {
                        for (int xFilterSpace = startX; xFilterSpace < RitualArae.this.multiblock.pattern()[1].length; xFilterSpace++) {
                            if (RitualArae.this.fluidSpaces.contains(RitualArae.this.multiblock().pattern()[yFilterSpace][zFilterSpace][xFilterSpace])) {
                                return new BlockPos(xFilterSpace, yFilterSpace, zFilterSpace);
                            }
                        }
                        startX = 0;
                    }
                    startZ = 0;
                }

                return null;
            }
        };
    }

    public @Nullable Fluid getFluid(Level level, BlockPos focusPosWorldSpace) {
        Fluid currentFluid = null;
        for (int yFilterSpace = 0; yFilterSpace < this.multiblock.pattern().length; yFilterSpace++) {
            for (int zFilterSpace = 0; zFilterSpace < this.multiblock.pattern()[0].length; zFilterSpace++) {
                for (int xFilterSpace = 0; xFilterSpace < this.multiblock.pattern()[1].length; xFilterSpace++) {
                    if (this.fluidSpaces.contains(this.multiblock().pattern()[yFilterSpace][zFilterSpace][xFilterSpace])) {
                        var fluidState = level.getFluidState(focusPosWorldSpace.subtract(this.multiblock().focusPosFilterSpace()).offset(xFilterSpace, yFilterSpace, zFilterSpace));
                        if ((fluidState.isSource() || fluidState.isEmpty()) && (currentFluid == null || currentFluid.isSame(fluidState.getType()))) {
                            if (currentFluid == null) {
                                currentFluid = fluidState.getType();
                            }
                        } else {
                            return null;
                        }
                    }
                }
            }
        }

        return currentFluid;
    }

    public List<ItemEntity> getItemsInFluidSpaces(Level level, BlockPos focusPosWorldSpace) {
        return level.getEntitiesOfClass(ItemEntity.class, this.multiblock().getAABB(focusPosWorldSpace)).stream()
                .filter(item -> {
                    BlockPos posFilterSpace = item.blockPosition().subtract(focusPosWorldSpace).offset(this.multiblock().focusPosFilterSpace());
                    return this.fluidSpaces().contains(this.multiblock().pattern()[posFilterSpace.getY()][posFilterSpace.getZ()][posFilterSpace.getX()]);
                }).toList();
    }
}
