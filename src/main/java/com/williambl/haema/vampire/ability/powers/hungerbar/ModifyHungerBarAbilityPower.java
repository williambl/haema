package com.williambl.haema.vampire.ability.powers.hungerbar;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.entity.LivingEntity;

public record ModifyHungerBarAbilityPower(
        ResourceLocation fullSprite,
        ResourceLocation halfSprite,
        ResourceLocation emptySprite,
        ResourceLocation hungerFullSprite,
        ResourceLocation hungerHalfSprite,
        ResourceLocation hungerEmptySprite,
        VExpression fullnessFunc) implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<ModifyHungerBarAbilityPower> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            ResourceLocation.CODEC.fieldOf("full_sprite").forGetter(ModifyHungerBarAbilityPower::fullSprite),
            ResourceLocation.CODEC.fieldOf("half_sprite").forGetter(ModifyHungerBarAbilityPower::halfSprite),
            ResourceLocation.CODEC.fieldOf("empty_sprite").forGetter(ModifyHungerBarAbilityPower::emptySprite),
            ResourceLocation.CODEC.fieldOf("hunger_full_sprite").forGetter(ModifyHungerBarAbilityPower::hungerFullSprite),
            ResourceLocation.CODEC.fieldOf("hunger_half_sprite").forGetter(ModifyHungerBarAbilityPower::hungerHalfSprite),
            ResourceLocation.CODEC.fieldOf("hunger_empty_sprite").forGetter(ModifyHungerBarAbilityPower::hungerEmptySprite),
            DFunctions.resolvedExpressionCodec(StandardVTypes.NUMBER, DFunctions.ENTITY).fieldOf("fullness").forGetter(ModifyHungerBarAbilityPower::fullnessFunc)
    ).apply(instance, ModifyHungerBarAbilityPower::new)));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public KeyDispatchDataCodec<ModifyHungerBarAbilityPower> codec() {
        return CODEC;
    }
}
