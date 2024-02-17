package com.williambl.haema.ritual.ritual;

import com.mojang.serialization.Codec;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.ritual.Ritual;
import com.williambl.haema.api.ritual.ritual.RitualAction;
import com.williambl.haema.api.ritual.ritual.RitualContainer;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.item.ItemEntity;

import java.util.ArrayList;
import java.util.stream.Collectors;

public record RemoveUsedItemsRitualAction() implements RitualAction {
    public static final KeyDispatchDataCodec<RemoveUsedItemsRitualAction> CODEC = KeyDispatchDataCodec.of(Codec.unit(RemoveUsedItemsRitualAction::new));

    @Override
    public void run(Ritual ritual, RitualContainer container) {
        HaemaUtil.allMatchOne(container.itemEntities().stream().map(ItemEntity::getItem).collect(Collectors.toCollection(ArrayList::new)), ritual.ingredients());
    }

    @Override
    public KeyDispatchDataCodec<? extends RitualAction> codec() {
        return CODEC;
    }
}
