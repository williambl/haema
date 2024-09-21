package com.williambl.haema.ritual.ritual;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.api.ritual.ritual.Ritual;
import com.williambl.haema.api.ritual.ritual.RitualAction;
import com.williambl.haema.api.ritual.ritual.RitualContainer;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.level.material.Fluid;

public record SetFluidRitualAction(Fluid fluid) implements RitualAction {
    public static final KeyDispatchDataCodec<SetFluidRitualAction> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            BuiltInRegistries.FLUID.byNameCodec().fieldOf("fluid").forGetter(SetFluidRitualAction::fluid)
    ).apply(instance, SetFluidRitualAction::new)));

    @Override
    public void run(Ritual ritual, RitualContainer container) {
        for (var pos : container.arae().getFluidSpaces(container.altarPos())) {
            container.level().setBlockAndUpdate(pos, this.fluid().defaultFluidState().createLegacyBlock());
        }
    }

    @Override
    public KeyDispatchDataCodec<? extends RitualAction> codec() {
        return CODEC;
    }
}
