package com.williambl.haema.client.vampire_mobs;

import com.williambl.haema.client.hunters.VampireHunterRenderer;
import com.williambl.haema.hunters.HaemaHunters;
import com.williambl.haema.vampire_mobs.HaemaVampireMobs;
import net.fabricmc.fabric.api.client.rendering.v1.EntityModelLayerRegistry;
import net.fabricmc.fabric.api.client.rendering.v1.EntityRendererRegistry;

public class HaemaVampireMobsClient {
    public static void init() {
        EntityModelLayerRegistry.registerModelLayer(VampiragerRenderer.VampiragerModel.LAYER, VampiragerRenderer.VampiragerModel::createBodyLayer);
        EntityRendererRegistry.register(HaemaVampireMobs.VampireMobEntityTypes.VAMPIRAGER, VampiragerRenderer::new);
    }
}
