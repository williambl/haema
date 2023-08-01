package com.williambl.haema.vampire.ability.powers;

import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import io.github.apace100.apoli.component.PowerHolderComponent;
import io.github.apace100.apoli.power.PowerTypeRegistry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

public record ApoliVampireAbilityPower(ResourceLocation powerName) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<ApoliVampireAbilityPower> CODEC = KeyDispatchDataCodec.of(ResourceLocation.CODEC.fieldOf("id")
            .xmap(ApoliVampireAbilityPower::new, ApoliVampireAbilityPower::powerName));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
        var sourceKey = entity.level().registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY).getKey(source);
        PowerHolderComponent.KEY.get(entity).addPower(PowerTypeRegistry.get(this.powerName), sourceKey);
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
        var sourceKey = entity.level().registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY).getKey(source);
        PowerHolderComponent.KEY.get(entity).removePower(PowerTypeRegistry.get(this.powerName), sourceKey);
    }

    @Override
    public KeyDispatchDataCodec<ApoliVampireAbilityPower> codec() {
        return CODEC;
    }
}
