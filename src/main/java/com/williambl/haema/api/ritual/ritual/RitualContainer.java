package com.williambl.haema.api.ritual.ritual;

import com.williambl.haema.api.ritual.RitualArae;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.material.Fluid;

import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

public class RitualContainer {
    private final Fluid fluid;
    private final BlockPos altarPos;
    private final RitualArae arae;
    private final Player player;
    private final Collection<ItemEntity> itemEntities;

    public RitualContainer(Collection<ItemEntity> itemEntities, Fluid fluid, BlockPos altarPos, RitualArae arae, Player player) {
        this.fluid = fluid;
        this.altarPos = altarPos;
        this.arae = arae;
        this.player = player;
        this.itemEntities = itemEntities;
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

    public Collection<ItemEntity> itemEntities() {
        return this.itemEntities;
    }

    public ArrayList<ItemStack> itemsCopy() {
        return this.itemEntities().stream().map(ItemEntity::getItem).map(ItemStack::copy).collect(Collectors.toCollection(ArrayList::new));
    }

    public ServerLevel level() {
        return (ServerLevel) this.player.level;
    }
}
