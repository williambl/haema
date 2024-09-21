package com.williambl.haema.client.vampire_mobs;

import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.entity.EntityRendererProvider;
import net.minecraft.client.renderer.entity.ZombieRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.monster.Zombie;

import static com.williambl.haema.Haema.id;

public class VampiricZombieRenderer extends ZombieRenderer {
    private static final ResourceLocation TEXTURE_LOCATION = id("textures/entity/vampiric_zombie.png");

    public VampiricZombieRenderer(EntityRendererProvider.Context context) {
        super(context);
    }

    @Override
    public ResourceLocation getTextureLocation(Zombie zombie) {
        return TEXTURE_LOCATION;
    }
}
