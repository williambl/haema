// TODO: No rats version for 1.20.1 yet (make sure to add back to haema_compat.mixins.json)
/*
package com.williambl.haema.compat.mixin.rats;

import com.williambl.haema.VampirableKt;
import com.williambl.haema.ability.AbilityModule;
import com.williambl.haema.api.VampireBurningEvents;
import com.williambl.haema.compat.rats.RatsMischiefIntegrationKt;
import com.williambl.haema.effect.SunlightSicknessEffect;
import com.williambl.haema.hunter.VampireHunterEntity;
import com.williambl.haema.vampiremobs.ConvertActiveTargetGoal;
import com.williambl.haema.vampiremobs.DrinkBloodActiveTargetGoal;
import ladysnake.ratsmischief.common.entity.RatEntity;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.ai.goal.ActiveTargetGoal;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.effect.StatusEffectInstance;
import net.minecraft.entity.passive.TameableEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.particle.DustParticleEffect;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(RatEntity.class)
public abstract class RatEntityMixin extends TameableEntity {
    @Shadow public abstract boolean damage(DamageSource source, float amount);

    protected RatEntityMixin(EntityType<? extends TameableEntity> entityType, World world) {
        super(entityType, world);
        VampirableKt.setAbilityLevel(this, AbilityModule.INSTANCE.getSTRENGTH(), 2);
    }

    @Inject(method = "initGoals", at = @At("TAIL"))
    void addVampireGoal(CallbackInfo ci) {
        targetSelector.add(7, new ActiveTargetGoal<>(this, VampireHunterEntity.class, 10, true, false, e -> VampirableKt.isVampire(this)));
        targetSelector.add(8, new DrinkBloodActiveTargetGoal<>(this, LivingEntity.class, true, false));
        targetSelector.add(9, new ConvertActiveTargetGoal<>(this, LivingEntity.class, true, false));
    }

    @Inject(method = "mobTick", at = @At("HEAD"))
    void vampireTick(CallbackInfo ci) {
        if (VampirableKt.isVampire(this)) {
            if (VampirableKt.isVampire(this) && !isDead()) {
                if (!this.world.isClient
                        && VampireBurningEvents.INSTANCE.getTRIGGER().invoker().willVampireBurn(this, world).get()
                        && VampireBurningEvents.INSTANCE.getVETO().invoker().willVampireBurn(this, world).get()
                ) {
                    this.addStatusEffect(new StatusEffectInstance(SunlightSicknessEffect.Companion.getInstance(), 10, 0, false, false, true));
                }
            }

            final var target = this.getTarget();

            if (target != null && VampirableKt.isVampire(target)) {
                setTarget(null);
            }
        }
    }

    @Override
    public boolean tryAttack(Entity target) {
        if (super.tryAttack(target) && VampirableKt.isVampire(this) && target instanceof LivingEntity lTarget) {
            ((ServerWorld) world).spawnParticles(
                    DustParticleEffect.DEFAULT,
                    target.getX(),
                    target.getY(),
                    target.getZ(),
                    10,
                    0.2,
                    0.2,
                    0.2,
                    0.5
            );

            if (VampirableKt.isVampirable(lTarget) && !VampirableKt.isVampire(lTarget)) {
                if (!(lTarget instanceof PlayerEntity) || this.world.getGameRules().getBoolean(RatsMischiefIntegrationKt.ratsCanConvertPlayers)) {
                        VampirableKt.convert(lTarget);
                        if (lTarget.getAttacker() == this) {
                            lTarget.setAttacker(null);
                        }
                    }
            } else if (!VampirableKt.isVampirable(lTarget)) {
                VampirableKt.getVampireComponent(this).feed(lTarget);
                this.setTarget(null);
            }
            return true;
        }
        return false;
    }
}
*/
