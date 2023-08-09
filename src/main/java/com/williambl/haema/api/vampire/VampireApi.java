package com.williambl.haema.api.vampire;

import net.minecraft.world.entity.Entity;

public class VampireApi {
    public static boolean isVampire(Entity entity) {
        return VampireComponent.KEY.maybeGet(entity).filter(VampireComponent::isVampire).isPresent();
    }
}
