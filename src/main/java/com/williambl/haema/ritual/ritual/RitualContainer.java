package com.williambl.haema.ritual.ritual;

import com.williambl.haema.api.ritual.RitualArae;
import net.minecraft.core.BlockPos;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.material.Fluid;

import java.util.Collection;

public class RitualContainer extends SimpleContainer {
    private final Fluid fluid;
    private final BlockPos altarPos;
    private final RitualArae arae;
    private final Player player;

    public RitualContainer(Collection<ItemEntity> itemEntities, Fluid fluid, BlockPos altarPos, RitualArae arae, Player player) {
        super(itemEntities.stream().map(ItemEntity::getItem).toArray(ItemStack[]::new));
        this.fluid = fluid;
        this.altarPos = altarPos;
        this.arae = arae;
        this.player = player;
    }

    public Fluid fluid() {
        return this.fluid;
    }

    public BlockPos altarPos() {
        return this.altarPos;
    }

    public RitualArae arae() {
        return this.arae;
    }

    public Player player() {
        return this.player;
    }
}
