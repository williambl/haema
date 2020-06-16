package com.williambl.haema.mixin;

import com.williambl.haema.Vampirable;
import com.williambl.haema.VampireBloodManager;
import com.williambl.haema.damagesource.DamageSourceExtensionsKt;
import com.williambl.haema.effect.SunlightSicknessEffect;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.effect.StatusEffectInstance;
import net.minecraft.entity.player.HungerManager;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(PlayerEntity.class)
public abstract class PlayerEntityMixin extends LivingEntity implements Vampirable {

    @Shadow protected HungerManager hungerManager;

    protected VampireBloodManager bloodManager = null; // to avoid a load of casts

    @Override
    public boolean isVampire() {
        return dataTracker.get(Vampirable.Companion.getIS_VAMPIRE());
    }
    @Override
    public void setVampire(boolean vampire) {
        dataTracker.set(Vampirable.Companion.getIS_VAMPIRE(), vampire);
        if (vampire) {
            checkBloodManager();
        }
    }

    protected PlayerEntityMixin(EntityType<? extends LivingEntity> entityType, World world) {
        super(entityType, world);
    }

    @Inject(method = "initDataTracker", at = @At("TAIL"))
    void initVampireTracker(CallbackInfo ci) {
        dataTracker.startTracking(Vampirable.Companion.getIS_VAMPIRE(), false);
        dataTracker.startTracking(Vampirable.Companion.getIS_KILLED(), false);
    }

    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;tick()V"))
    void vampireTick(CallbackInfo ci) {
        if (isVampire()) {
            checkBloodManager();

            if (this.isInDaylight()) {
                this.addStatusEffect(new StatusEffectInstance(SunlightSicknessEffect.Companion.getInstance(), 5, 0));
            }

            if (this.getHealth() <= 0 && !this.isDead()) {
                bloodManager.removeBlood(0.005);
            }
        }
    }

    @Redirect(method = "damage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;damage(Lnet/minecraft/entity/damage/DamageSource;F)Z"))
    boolean damage(LivingEntity livingEntity, DamageSource source, float amount) {
        if (isVampire()) {
            boolean isDamageSourceEffective = DamageSourceExtensionsKt.isEffectiveAgainstVampires(source);
            if (isDamageSourceEffective)
                amount *= 2;

            float bloodAbsorptionAmount = 0.5f * amount;
            bloodAbsorptionAmount = bloodManager.getAbsoluteBloodLevel() > bloodAbsorptionAmount ?
                    bloodAbsorptionAmount
                    : (float) bloodManager.getAbsoluteBloodLevel();
            bloodManager.removeBlood((1.1-Math.pow(bloodManager.getBloodLevel()/20.0, 2.0))*bloodAbsorptionAmount);

            boolean result = super.damage(source, amount - bloodAbsorptionAmount);
            dataTracker.set(Vampirable.Companion.getIS_KILLED(), this.getHealth() <= 0 && isDamageSourceEffective);
            return result;
        }
        return super.damage(source, amount);
    }

    @Inject(method = "readCustomDataFromTag", at = @At("TAIL"))
    void readVampireData(CompoundTag tag, CallbackInfo ci) {
        this.setVampire(tag.getBoolean("IsVampire"));
    }

    @Inject(method = "writeCustomDataToTag", at = @At("TAIL"))
    void writeVampireData(CompoundTag tag, CallbackInfo ci) {
        tag.putBoolean("IsVampire", isVampire());
    }

    @Override
    public boolean isDead() {
        if (isVampire())
            return super.isDead() && bloodManager.getBloodLevel() <= 0 && dataTracker.get(Vampirable.Companion.getIS_KILLED());
        return super.isDead();
    }

    protected boolean isInDaylight() {
        return !this.world.isClient && this.world.isDay() && !this.world.isRaining() && this.world.isSkyVisible(this.getBlockPos());
    }

    @Override
    public void checkBloodManager() {
        if (bloodManager == null) {
            hungerManager = new VampireBloodManager();
            bloodManager = (VampireBloodManager) hungerManager;
        }
    }
}
