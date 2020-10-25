package com.williambl.haema.mixin;

import com.williambl.haema.Vampirable;
import com.williambl.haema.VampireBloodManager;
import com.williambl.haema.damagesource.DamageSourceExtensionsKt;
import com.williambl.haema.effect.SunlightSicknessEffect;
import com.williambl.haema.util.HaemaGameRulesKt;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.attribute.EntityAttributes;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.effect.StatusEffectInstance;
import net.minecraft.entity.player.HungerManager;
import net.minecraft.entity.player.PlayerAbilities;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(PlayerEntity.class)
public abstract class PlayerEntityMixin extends LivingEntity implements Vampirable {

    @Shadow protected HungerManager hungerManager;

    @Shadow @Final public PlayerAbilities abilities;
    protected VampireBloodManager bloodManager = null; // to avoid a load of casts

    protected PlayerEntityMixin(EntityType<? extends LivingEntity> entityType, World world) {
        super(entityType, world);
    }

    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;tick()V"))
    void vampireTick(CallbackInfo ci) {
        if (isVampire()) {
            checkBloodManager();

            if (world.getGameRules().get(HaemaGameRulesKt.getVampiresBurn()).get() && this.isInDaylight() && !abilities.creativeMode) {
                this.addStatusEffect(new StatusEffectInstance(SunlightSicknessEffect.Companion.getInstance(), 5, 0));
            }
        }
    }

    @Redirect(method = "damage", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;damage(Lnet/minecraft/entity/damage/DamageSource;F)Z"))
    boolean damage(LivingEntity livingEntity, DamageSource source, float amount) {
        if (isVampire()) {
            boolean isDamageSourceEffective = DamageSourceExtensionsKt.isEffectiveAgainstVampires(source);
            if (isDamageSourceEffective)
                amount *= 1.25;

            float bloodAbsorptionAmount = 0.5f * amount;
            bloodAbsorptionAmount = bloodManager.getAbsoluteBloodLevel() > bloodAbsorptionAmount ?
                    bloodAbsorptionAmount
                    : (float) bloodManager.getAbsoluteBloodLevel();

            double originalMax = getAttributeBaseValue(EntityAttributes.GENERIC_MAX_HEALTH);
            if (getHealth()-(amount-bloodAbsorptionAmount) > originalMax)
                bloodManager.removeBlood(Math.min(bloodAbsorptionAmount, getHealth()-originalMax));

            boolean result = super.damage(source, amount - bloodAbsorptionAmount);
            this.setKilled(this.getHealth() <= 0 && isDamageSourceEffective);
            return result;
        }
        return super.damage(source, amount);
    }

    @Override
    public boolean isDead() {
        if (isVampire() && bloodManager != null)
            return super.isDead() && bloodManager.getBloodLevel() <= 0 && isKilled();
        return super.isDead();
    }

    @Override
    public boolean isAlive() {
        return isVampire() ? !isDead() : super.isAlive();
    }

    @Unique
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
