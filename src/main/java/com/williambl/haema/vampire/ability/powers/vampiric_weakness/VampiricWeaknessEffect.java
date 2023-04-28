package com.williambl.haema.vampire.ability.powers.vampiric_weakness;

import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectCategory;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.AttributeMap;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.jetbrains.annotations.NotNull;

public class VampiricWeaknessEffect extends MobEffect {
    public VampiricWeaknessEffect() {
        super(MobEffectCategory.HARMFUL, 0xab0c0c00);
        this.addAttributeModifier(
                        Attributes.ATTACK_DAMAGE,
                        "beb69dfa-5de3-4f82-82a5-29f5ba715a18",
                        -4.0,
                        AttributeModifier.Operation.ADDITION
                )
                .addAttributeModifier(
                        Attributes.ATTACK_SPEED,
                        "3ca8311c-601a-44f9-97ee-2b0677247e64",
                        -0.25,
                        AttributeModifier.Operation.MULTIPLY_TOTAL
                )
                .addAttributeModifier(
                        Attributes.MAX_HEALTH,
                        "858a6a28-5092-49ea-a94e-eb74db018a92",
                        -2.0,
                        AttributeModifier.Operation.ADDITION
                )
                .addAttributeModifier(
                        Attributes.MOVEMENT_SPEED,
                        "7a47b1b8-16a5-4877-905a-07ffd5d2189b",
                        -0.15,
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

    @Override
    public void addAttributeModifiers(@NotNull LivingEntity entity, @NotNull AttributeMap attributes, int amplifier) {
        super.addAttributeModifiers(entity, attributes, amplifier);
        if (entity.getHealth() >= entity.getMaxHealth()) {
            entity.setHealth(entity.getMaxHealth());
        }
    }
}
