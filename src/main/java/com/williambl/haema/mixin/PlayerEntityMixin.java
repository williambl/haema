package com.williambl.haema.mixin;

import com.williambl.haema.Vampirable;
import com.williambl.haema.VampireBloodManager;
import com.williambl.haema.damagesource.DamageSourceExtensionsKt;
import com.williambl.haema.effect.SunlightSicknessEffect;
import com.williambl.haema.util.HaemaGameRulesKt;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.effect.StatusEffectInstance;
import net.minecraft.entity.player.HungerManager;
import net.minecraft.entity.player.PlayerAbilities;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

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

    @Inject(method = "damage", at = @At(value = "INVOKE_ASSIGN", target = "Lnet/minecraft/entity/LivingEntity;damage(Lnet/minecraft/entity/damage/DamageSource;F)Z"))
    void damage(DamageSource source, float amount, CallbackInfoReturnable<Boolean> cir) {
        if (isVampire()) {
            boolean isDamageSourceEffective = DamageSourceExtensionsKt.isEffectiveAgainstVampires(source);
            this.setKilled(this.getHealth() <= 0 && isDamageSourceEffective);
        }
    }

    @Redirect(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;isDay()Z"))
    boolean allowVampiresToSleepInDay(World world) {
        return world.isDay() && !this.isVampire();
    }

    @Redirect(method = "isInvulnerableTo", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/GameRules;getBoolean(Lnet/minecraft/world/GameRules$Key;)Z"))
    boolean makeVampiresImmuneToFalling(GameRules gameRules, GameRules.Key<GameRules.BooleanRule> rule) {
        return gameRules.getBoolean(rule) && !isVampire();
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
