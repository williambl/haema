package com.williambl.haema.mixin;

import com.williambl.haema.VampirableKt;
import com.williambl.haema.ability.AbilityModule;
import com.williambl.haema.ability.component.mist_form.MistFormAbilityComponent;
import com.williambl.haema.api.VampireBurningEvents;
import com.williambl.haema.damagesource.DamageSourceModule;
import com.williambl.haema.effect.SunlightSicknessEffect;
import com.williambl.haema.util.HaemaGameRules;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.effect.StatusEffectInstance;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.GameMode;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.ModifyVariable;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(PlayerEntity.class)
public abstract class PlayerEntityMixin extends LivingEntity {
    @Shadow public abstract void setAbsorptionAmount(float amount);

    @Shadow public abstract float getAbsorptionAmount();

    protected NbtCompound nbt;

    protected PlayerEntityMixin(EntityType<? extends LivingEntity> entityType, World world) {
        super(entityType, world);
    }

    @Inject(method = "readCustomDataFromNbt", at = @At("RETURN"))
    void addVampireHungerManager(NbtCompound tag, CallbackInfo ci) {
        if (tag.contains("foodLevel")) {
            nbt = tag;
        }
    }

    @Inject(method = "isBlockBreakingRestricted", at = @At("HEAD"), cancellable = true)
    void mistFormCannotBreakBlocks(World world, BlockPos pos, GameMode gameMode, CallbackInfoReturnable<Boolean> cir) {
        if (MistFormAbilityComponent.Companion.getEntityKey().get(this).isInMistForm()) {
            cir.setReturnValue(true);
        }
    }

    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;tick()V"))
    void vampireTick(CallbackInfo ci) {
        if (!Float.isFinite(getHealth()) || !Float.isFinite(getAbsorptionAmount()) || getHealth() < 0 || getAbsorptionAmount() < 0) {
            setAbsorptionAmount(0.0f);
            setHealth(0.0f);
        }
        if (VampirableKt.isVampire(this) && !isDead()) {
            if (!this.world.isClient
                    && VampireBurningEvents.INSTANCE.getTRIGGER().invoker().willVampireBurn((PlayerEntity) (Object) this, world).get()
                    && VampireBurningEvents.INSTANCE.getVETO().invoker().willVampireBurn((PlayerEntity) (Object) this, world).get()
            ) {
                this.addStatusEffect(new StatusEffectInstance(SunlightSicknessEffect.Companion.getInstance(), 10, 0, false, false, true));
            }
        }
    }

    @ModifyVariable(method = "applyDamage",
            at = @At(value = "INVOKE_ASSIGN", target = "Lnet/minecraft/entity/player/PlayerEntity;applyEnchantmentsToDamage(Lnet/minecraft/entity/damage/DamageSource;F)F"),
            argsOnly = true,
            require = 1,
            index = 2
    )
    float tweakDamageIfVampire(float amount, DamageSource source) {
        float result = VampirableKt.isVampire(this) && DamageSourceModule.INSTANCE.isEffectiveAgainstVampires(source, world) ?
                amount * 1.25f
                : amount;

        return Float.isFinite(result) ? result : amount;
    }

    @Redirect(method = "isInvulnerableTo", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/GameRules;getBoolean(Lnet/minecraft/world/GameRules$Key;)Z", ordinal = 1))
    boolean makeVampiresImmuneToFalling(GameRules gameRules, GameRules.Key<GameRules.BooleanRule> rule) {
        return gameRules.getBoolean(rule) && !(VampirableKt.isVampire(this) && VampirableKt.getAbilityLevel(this, AbilityModule.INSTANCE.getDASH()) >= 3);
    }

    @Redirect(method = "isInvulnerableTo", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/GameRules;getBoolean(Lnet/minecraft/world/GameRules$Key;)Z", ordinal = 0))
    boolean makeVampiresImmuneToDrowning(GameRules gameRules, GameRules.Key<GameRules.BooleanRule> rule) {
        return gameRules.getBoolean(rule) && (!VampirableKt.isVampire(this) || gameRules.getBoolean(HaemaGameRules.INSTANCE.getVampiresDrown()));
    }

    @Override
    public boolean isDead() {
        if (VampirableKt.isVampire(this) && VampirableKt.getAbilityLevel(this, AbilityModule.INSTANCE.getIMMORTALITY()) > 0)
            return super.isDead() && VampirableKt.isKilled(this);
        return super.isDead();
    }

    @Override
    public boolean isAlive() {
        return VampirableKt.isVampire(this) ? !isDead() : super.isAlive();
    }
}
