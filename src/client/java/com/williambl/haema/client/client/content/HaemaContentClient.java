package com.williambl.haema.client.client.content;

import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodQuality;
import net.fabricmc.fabric.api.client.render.fluid.v1.FluidRenderHandlerRegistry;
import net.fabricmc.fabric.api.client.render.fluid.v1.SimpleFluidRenderHandler;

public class HaemaContentClient {
    public static void init() {
        Fluids.init();
    }

    public static class Fluids {
        public static void init() {
            for (var quality : BloodQuality.values()) {
                FluidRenderHandlerRegistry.INSTANCE.register(HaemaContent.Fluids.BLOOD.get(quality), HaemaContent.Fluids.FLOWING_BLOOD.get(quality), SimpleFluidRenderHandler.coloredWater(0xcd050e));
            }
        }
    }
}
