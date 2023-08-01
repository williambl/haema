package com.williambl.haema.content.injector;

import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectCategory;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.AttributeMap;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.player.Player;
import org.jetbrains.annotations.NotNull;

import static com.williambl.haema.HaemaUtil.isInteger;

public class IncompatibleBloodEffect extends MobEffect {
    private static final String ATTRIBUTE_MODIFIER_NAME = "Haema incompatible blood";
    public IncompatibleBloodEffect() {
        super(MobEffectCategory.HARMFUL, 0x5c2228);
    }

    @Override
    public void applyEffectTick(@NotNull LivingEntity entity, int amplifier) {
        if (entity instanceof Player p && p.isCreative()) {
            this.removeAttributeModifiers(entity, entity.getAttributes(), amplifier);
            return;
        }

        if (entity.level().isClientSide()) {
            return;
        }

        var attr = entity.getAttribute(Attributes.MAX_HEALTH);
        if (attr == null) { // how
            return;
        }

        int maxModifier = attr.getModifiers().stream()
                .map(AttributeModifier::getName)
                .filter(m -> m.contains(ATTRIBUTE_MODIFIER_NAME) && isInteger(""+m.charAt(m.length()-1), 16))
                .mapToInt(m -> Integer.parseInt(""+m.charAt(m.length()-1), 16))
                .max()
                .orElse(0);

        if (maxModifier < 15 && attr.getValue() > 2) {
            attr.addTransientModifier(new AttributeModifier(ATTRIBUTE_MODIFIER_NAME + "_"+Integer.toString(maxModifier+1, 16), -4, AttributeModifier.Operation.ADDITION));
            if (entity.getHealth() > entity.getMaxHealth()) {
                entity.setHealth(entity.getMaxHealth());
            }
            entity.hasImpulse = true; // force sync the attribute change
        } else {
            entity.hurt(entity.damageSources().magic(), 4f); //TODO custom damage source
        }
    }

    @Override
    public boolean isDurationEffectTick(int duration, int amplifier) {
        int k = 40 >> amplifier;
        if (k > 0) {
            return duration % k == 0;
        } else {
            return true;
        }
    }

    @Override
    public void removeAttributeModifiers(LivingEntity livingEntity, AttributeMap attributeMap, int i) {
        super.removeAttributeModifiers(livingEntity, attributeMap, i);
    }
}
