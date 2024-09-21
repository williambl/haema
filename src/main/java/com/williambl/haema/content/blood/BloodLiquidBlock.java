package com.williambl.haema.content.blood;

import com.williambl.haema.api.content.blood.BloodQuality;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.level.block.LiquidBlock;
import net.minecraft.world.level.material.FlowingFluid;

public class BloodLiquidBlock extends LiquidBlock {
    private final BloodQuality quality;

    public BloodLiquidBlock(FlowingFluid flowingFluid, BloodQuality quality, Properties properties) {
        super(flowingFluid, properties);
        this.quality = quality;
    }

    @Override
    public MutableComponent getName() {
        return Component.translatable(this.getDescriptionId(), this.quality.text());
    }
}
