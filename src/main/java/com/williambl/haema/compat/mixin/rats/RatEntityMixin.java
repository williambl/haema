package com.williambl.haema.compat.mixin.rats;

import com.williambl.haema.VampirableKt;
import com.williambl.haema.compat.rats.VampiRatAttackGoal;
import com.williambl.haema.damagesource.SunlightDamageSource;
import com.williambl.haema.effect.VampiricStrengthEffect;
import com.williambl.haema.util.HaemaGameRules;
import ladysnake.ratsmischief.common.entity.RatEntity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.ai.goal.ActiveTargetGoal;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.passive.TameableEntity;
import net.minecraft.particle.ParticleTypes;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

//TODO: improve behaviour, make use of blood
@Mixin(RatEntity.class)
public abstract class RatEntityMixin extends TameableEntity {
    @Shadow public abstract boolean damage(DamageSource source, float amount);

    protected RatEntityMixin(EntityType<? extends TameableEntity> entityType, World world) {
        super(entityType, world);
    }

    @Inject(method = "initGoals", at = @At("TAIL"))
    void addVampireGoal(CallbackInfo ci) {
        goalSelector.add(3, new VampiRatAttackGoal((RatEntity) (Object) this, 1.0, true));

        targetSelector.add(8, new ActiveTargetGoal<>(this, LivingEntity.class, 10, true, false, (livingEntity) ->
                VampirableKt.isVampire(this) && !VampirableKt.isVampirable(livingEntity) && !this.hasStatusEffect(VampiricStrengthEffect.Companion.getInstance())
        ));
        targetSelector.add(9, new ActiveTargetGoal<>(this, LivingEntity.class, 10, true, false, (livingEntity) ->
                VampirableKt.isVampire(this) && VampirableKt.isVampirable(livingEntity) && !VampirableKt.isVampire(livingEntity)
        ));
    }

    @Inject(method = "mobTick", at = @At("HEAD"))
    void vampireTick(CallbackInfo ci) {
        if (VampirableKt.isVampire(this)) {
            if (world.isDay() && !world.isRaining() && world.isSkyVisible(getBlockPos()) && world.getGameRules().getBoolean(HaemaGameRules.INSTANCE.getVampiresBurn())) {
                if (age % 10 == 0) {
                    damage(SunlightDamageSource.Companion.getInstance(), 0.2f);
                    ((ServerWorld) world).spawnParticles(ParticleTypes.FLAME, getX() - 0.5, getY(), getZ() - 0.5, 20, 0.2, 0.2, 0.2, 0.1);
                }
            }

            if (VampirableKt.isVampire(getTarget())) {
                setTarget(null);
            }
        }
    }
}
