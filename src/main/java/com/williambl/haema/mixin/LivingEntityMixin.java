package com.williambl.haema.mixin;

import com.williambl.haema.HaemaKt;
import com.williambl.haema.Vampirable;
import com.williambl.haema.damagesource.BloodLossDamageSource;
import com.williambl.haema.damagesource.DamageSourceExtensionsKt;
import com.williambl.haema.util.HaemaGameRulesKt;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.particle.ParticleTypes;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(LivingEntity.class)
public abstract class LivingEntityMixin extends Entity {

    public LivingEntityMixin(EntityType<?> type, World world) {
        super(type, world);
    }

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

    @Inject(method = "onDeath", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;setPose(Lnet/minecraft/entity/EntityPose;)V"))
    void alertVampireHuntersToDeath(DamageSource source, CallbackInfo ci) {
        if (source == BloodLossDamageSource.Companion.getInstance()) {
            for (float i = 0f; i < 0.5f; i += 0.1f) {
                world.addImportantParticle(ParticleTypes.CAMPFIRE_SIGNAL_SMOKE, getX(), getY(), getZ(), 0.0, 2.0 * i, 0.0);
            }

            if (world instanceof ServerWorld) {
                if (random.nextDouble() < world.getGameRules().get(HaemaGameRulesKt.getVampireHunterNoticeChance()).get())
                    HaemaKt.getVampireHunterSpawner().trySpawnNear((ServerWorld)world, random, getBlockPos());
            }
        }
    }
}
