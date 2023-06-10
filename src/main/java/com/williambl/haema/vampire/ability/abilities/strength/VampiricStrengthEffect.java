package com.williambl.haema.vampire.ability.abilities.strength;

import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectCategory;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.jetbrains.annotations.NotNull;

public class VampiricStrengthEffect extends MobEffect {
    public VampiricStrengthEffect() {
        super(MobEffectCategory.HARMFUL, 0xab0c0c00);
        this.addAttributeModifier(
                        Attributes.ATTACK_DAMAGE,
                        "98e17081-0f5c-44b2-a65d-53072c3ae004",
                        4.0,
                        AttributeModifier.Operation.ADDITION
                )
                .addAttributeModifier(
                        Attributes.ATTACK_SPEED,
                        "7ecd14c4-5fac-422e-92af-175f0df693b0",
                        0.3,
                        AttributeModifier.Operation.MULTIPLY_TOTAL
                )
                .addAttributeModifier(
                        Attributes.MOVEMENT_SPEED,
                        "dffdb8ef-0746-4d44-983b-bad848da128a",
                        0.2,
                        AttributeModifier.Operation.MULTIPLY_TOTAL
                );
    }

    @Override
    public void applyEffectTick(@NotNull LivingEntity entity, int amplifier) {
    }

    @Override
    public boolean isDurationEffectTick(int duration, int amplifier) {
        return false;
    }
}
