package com.williambl.haema.mixin;

import com.williambl.haema.Vampirable;
import com.williambl.haema.damagesource.BloodLossDamageSource;
import com.williambl.haema.damagesource.DamageSourceExtensionsKt;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(LivingEntity.class)
public abstract class LivingEntityMixin {

    @Shadow public abstract float getHealth();

    @Shadow public abstract boolean isDead();

    @Unique private DamageSource currentSource;

    @Inject(method = "damage", at = @At(value = "HEAD"))
    void setCurrentSource(DamageSource source, float amount, CallbackInfoReturnable<Boolean> cir) {
        currentSource = source;
    }

    @Redirect(method = "damage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;isDead()Z", ordinal = 1))
    boolean isActuallyDead(LivingEntity livingEntity) {
        if (livingEntity instanceof PlayerEntity && ((Vampirable)livingEntity).isVampire()) {
            DamageSource theCurrentSource = currentSource;
            currentSource = null;
            return this.getHealth() <= 0 && DamageSourceExtensionsKt.isEffectiveAgainstVampires(theCurrentSource);
        }
        return this.isDead();
    }

    @Redirect(method = "damage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;isSleeping()Z"))
    boolean dontWakeForFeeding(LivingEntity livingEntity) {
        return livingEntity.isSleeping() && currentSource != BloodLossDamageSource.Companion.getInstance();
    }
}
