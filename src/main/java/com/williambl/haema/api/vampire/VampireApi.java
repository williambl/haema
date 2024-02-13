package com.williambl.haema.api.vampire;

import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import net.minecraft.world.entity.Entity;

public class VampireApi {
    public static boolean isVampire(Entity entity) {
        return VampireComponent.KEY.maybeGet(entity).filter(VampireComponent::isVampire).isPresent();
    }

    public static boolean setActiveAbilityOnServer(Entity entity, VampireAbility ability) {
        return !entity.level().isClientSide
                && VampireAbilitiesComponent.KEY.maybeGet(entity).map(c -> c.setActiveAbility(ability)).orElse(false);
    }
}
