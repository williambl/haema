package com.williambl.haema.client.client.content;

import com.williambl.haema.Haema;
import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.content.HaemaContent;
import net.fabricmc.fabric.api.client.render.fluid.v1.FluidRenderHandlerRegistry;
import net.fabricmc.fabric.api.client.render.fluid.v1.SimpleFluidRenderHandler;
import net.fabricmc.fabric.api.client.rendering.v1.ColorProviderRegistry;
import net.fabricmc.fabric.api.transfer.v1.client.fluid.FluidVariantRendering;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;

public class HaemaContentClient {
    public static void init() {
        Fluids.init();
    }

    public static class Fluids {
        @SuppressWarnings("UnstableApiUsage")
        public static void init() {
            for (var quality : BloodQuality.values()) {
                FluidRenderHandlerRegistry.INSTANCE.register(HaemaContent.Fluids.BLOOD.get(quality), HaemaContent.Fluids.FLOWING_BLOOD.get(quality), SimpleFluidRenderHandler.coloredWater(Haema.HOMESTUCK_MODE ? quality.colour : 0xcd050e));
                ColorProviderRegistry.BLOCK.register(
                        (blockState, blockAndTintGetter, blockPos, i) -> FluidVariantRendering.getColor(FluidVariant.of(HaemaContent.Fluids.BLOOD.get(quality))),
                        HaemaContent.Fluids.BLOOD_CAULDRON.get(quality)
                );
            }
        }
    }
}
