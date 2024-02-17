package com.williambl.haema.client.hunters;

import com.williambl.haema.hunters.HaemaHunters;
import net.fabricmc.fabric.api.client.rendering.v1.EntityModelLayerRegistry;
import net.fabricmc.fabric.api.client.rendering.v1.EntityRendererRegistry;

public class HaemaHuntersClient {
    public static void init() {
        EntityModelLayerRegistry.registerModelLayer(VampireHunterRenderer.VampireHunterModel.LAYER, VampireHunterRenderer.VampireHunterModel::createBodyLayer);
        EntityRendererRegistry.register(HaemaHunters.HunterEntityTypes.VAMPIRE_HUNTER, ctx -> new VampireHunterRenderer(ctx));
    }
}
