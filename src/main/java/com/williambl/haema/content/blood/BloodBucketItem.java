package com.williambl.haema.content.blood;

import net.minecraft.network.chat.Component;
import net.minecraft.world.item.BucketItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.material.Fluid;

public class BloodBucketItem extends BucketItem {
    private final BloodQuality quality;

    public BloodBucketItem(Fluid fluid, BloodQuality quality, Properties properties) {
        super(fluid, properties);
        this.quality = quality;
    }

    @Override
    public Component getName(ItemStack stack) {
        return Component.translatable(this.getDescriptionId(stack), this.quality.text());
    }
}
