package com.williambl.haema.mixin;

import com.williambl.haema.Vampirable;
import com.williambl.haema.VampireBloodManager;
import com.williambl.haema.abilities.VampireAbility;
import com.williambl.haema.criteria.VampireHunterTriggerCriterion;
import com.williambl.haema.damagesource.BloodLossDamageSource;
import com.williambl.haema.damagesource.DamageSourcesKt;
import com.williambl.haema.hunter.VampireHunterSpawner;
import com.williambl.haema.util.HaemaGameRulesKt;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.particle.DustParticleEffect;
import net.minecraft.server.network.ServerPlayerEntity;
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

    @Unique private DamageSource currentSource;

    @Inject(method = "damage", at = @At(value = "HEAD"))
    void setCurrentSource(DamageSource source, float amount, CallbackInfoReturnable<Boolean> cir) {
        currentSource = source;
    }

    @Inject(method = "damage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;isDead()Z", ordinal = 1))
    void haema$setDeadVampireAsKilled(DamageSource source, float amount, CallbackInfoReturnable<Boolean> cir) {
        //noinspection ConstantConditions
        if (((Object) this) instanceof PlayerEntity && ((Vampirable) this).isVampire()
                && source != null && ((Vampirable) this).getAbilityLevel(VampireAbility.Companion.getIMMORTALITY()) > 0) {
            if (this.getHealth() <= 0 && DamageSourcesKt.isEffectiveAgainstVampires(source, this.world)) {
                ((VampireBloodManager) ((PlayerEntity) (Object) this).getHungerManager()).setAbsoluteBloodLevel(0.0);
                ((Vampirable) this).setKilled(true);
            }
        }
    }

    @Inject(method = "tryUseTotem", at = @At(value = "INVOKE", target = "Lnet/minecraft/util/Hand;values()[Lnet/minecraft/util/Hand;"), cancellable = true)
    void haema$keepVampireAlive(DamageSource source, CallbackInfoReturnable<Boolean> cir) {
        //noinspection ConstantConditions
        if (((Object) this) instanceof PlayerEntity && ((Vampirable)this).isVampire()
                && source != null && ((Vampirable)this).getAbilityLevel(VampireAbility.Companion.getIMMORTALITY()) > 0) {
            if (!(this.getHealth() <= 0 && DamageSourcesKt.isEffectiveAgainstVampires(source, this.world))) {
                cir.setReturnValue(true);
            }
        }
    }

    @Redirect(method = "damage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;isSleeping()Z"))
    boolean dontWakeForFeeding(LivingEntity livingEntity) {
        return livingEntity.isSleeping() && currentSource != BloodLossDamageSource.Companion.getInstance();
    }

    @Inject(method = "onDeath", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;setPose(Lnet/minecraft/entity/EntityPose;)V"))
    void alertVampireHuntersToDeath(DamageSource source, CallbackInfo ci) {
        if (source == BloodLossDamageSource.Companion.getInstance() && world instanceof ServerWorld) {
            ((ServerWorld) world).spawnParticles(new DustParticleEffect(DustParticleEffect.RED, 3.0f), getX(), getY()+1, getZ(), 30, 1.0, 1.0, 1.0, 0.1);

            if (random.nextDouble() < world.getGameRules().get(HaemaGameRulesKt.getVampireHunterNoticeChance()).get()) {
                //noinspection ConstantConditions
                if ((Object) this instanceof ServerPlayerEntity) {
                    VampireHunterTriggerCriterion.INSTANCE.trigger((ServerPlayerEntity) (Object) this);
                }
                VampireHunterSpawner.Companion.getInstance().trySpawnNear((ServerWorld) world, random, getBlockPos());
            }
        }
    }
}
