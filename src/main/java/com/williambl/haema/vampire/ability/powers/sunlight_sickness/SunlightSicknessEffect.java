package com.williambl.haema.vampire.ability.powers.sunlight_sickness;

import com.williambl.haema.api.vampire.VampireComponent;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectCategory;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.player.Player;
import org.jetbrains.annotations.NotNull;

public class SunlightSicknessEffect extends MobEffect {
    public SunlightSicknessEffect() {
        super(MobEffectCategory.HARMFUL, 0xf5a74200);
        this.addAttributeModifier(
                Attributes.ATTACK_DAMAGE,
                "c85d1cfe-2c10-4d25-b650-49c045979842",
                -4.0,
                AttributeModifier.Operation.ADDITION);
    }

    @Override
    public void applyEffectTick(@NotNull LivingEntity entity, int amplifier) {
        if (entity instanceof Player p && p.isCreative()) {
            return;
        }

        var component = VampireComponent.KEY.getNullable(entity);
        if (component == null) {
            return;
        }

        if (entity.tickCount % 10 == 0) {
            entity.hurt(entity.damageSources().inFire(), 0.5f + amplifier); //TODO damage source
            if (component.isVampire()) {
                component.setBlood(component.getBlood() - 0.25);
            }
            if (entity.level() instanceof ServerLevel level) {
                level.sendParticles(
                        ParticleTypes.FLAME,
                        entity.getX() - entity.getBbWidth() / 4.0,
                        entity.getY(),
                        entity.getZ() - entity.getBbWidth() / 4.0,
                        10,
                        entity.getBbWidth()/2.0,
                        entity.getBbHeight()/2.0,
                        entity.getBbWidth()/2.0,
                        0.0
                );
            }
        }
    }

    @Override
    public boolean shouldApplyEffectTickThisTick(int duration, int amplifier) {
        return true;
    }
}
