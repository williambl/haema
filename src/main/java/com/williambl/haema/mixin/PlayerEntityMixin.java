package com.williambl.haema.mixin;

import com.williambl.haema.Vampirable;
import com.williambl.haema.VampireBloodManager;
import com.williambl.haema.ability.AbilityModule;
import com.williambl.haema.api.VampireBurningEvents;
import com.williambl.haema.damagesource.DamageSourceModule;
import com.williambl.haema.effect.SunlightSicknessEffect;
import com.williambl.haema.util.HaemaGameRulesKt;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.effect.StatusEffectInstance;
import net.minecraft.entity.player.HungerManager;
import net.minecraft.entity.player.PlayerAbilities;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.world.GameRules;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.ModifyVariable;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(PlayerEntity.class)
public abstract class PlayerEntityMixin extends LivingEntity implements Vampirable {

    @Shadow protected HungerManager hungerManager;

    @Shadow @Final public PlayerAbilities abilities;

    @Shadow public abstract void setAbsorptionAmount(float amount);

    @Shadow public abstract float getAbsorptionAmount();

    protected VampireBloodManager bloodManager = null; // to avoid a load of casts
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

    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;tick()V"))
    void vampireTick(CallbackInfo ci) {
        if (!Float.isFinite(getHealth()) || !Float.isFinite(getAbsorptionAmount()) || getHealth() < 0 || getAbsorptionAmount() < 0) {
            setAbsorptionAmount(0.0f);
            setHealth(0.0f);
        }
        if (isVampire() && !isDead()) {
            checkBloodManager();

            if (!this.world.isClient
                    && VampireBurningEvents.INSTANCE.getTRIGGER().invoker().willVampireBurn((PlayerEntity) (Object) this, world).get()
                    && VampireBurningEvents.INSTANCE.getVETO().invoker().willVampireBurn((PlayerEntity) (Object) this, world).get()
            ) {
                this.addStatusEffect(new StatusEffectInstance(SunlightSicknessEffect.Companion.getInstance(), 5, 0));
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
        float result = this.isVampire() && DamageSourceModule.INSTANCE.isEffectiveAgainstVampires(source, world) ?
                amount * 1.25f
                : amount;

        return Float.isFinite(result) ? result : amount;
    }

    @Redirect(method = "isInvulnerableTo", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/GameRules;getBoolean(Lnet/minecraft/world/GameRules$Key;)Z", ordinal = 1))
    boolean makeVampiresImmuneToFalling(GameRules gameRules, GameRules.Key<GameRules.BooleanRule> rule) {
        return gameRules.getBoolean(rule) && !(isVampire() && getAbilityLevel(AbilityModule.INSTANCE.getDASH()) >= 3);
    }

    @Redirect(method = "isInvulnerableTo", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/GameRules;getBoolean(Lnet/minecraft/world/GameRules$Key;)Z", ordinal = 0))
    boolean makeVampiresImmuneToDrowning(GameRules gameRules, GameRules.Key<GameRules.BooleanRule> rule) {
        return gameRules.getBoolean(rule) && (!isVampire() || gameRules.getBoolean(HaemaGameRulesKt.getVampiresDrown()));
    }

    @Override
    public boolean isDead() {
        if (isVampire() && getAbilityLevel(AbilityModule.INSTANCE.getIMMORTALITY()) > 0)
            return super.isDead() && isKilled();
        return super.isDead();
    }

    @Override
    public boolean isAlive() {
        return isVampire() ? !isDead() : super.isAlive();
    }

    @Override
    public void checkBloodManager() {
        if (bloodManager == null) {
            hungerManager = new VampireBloodManager();
            bloodManager = (VampireBloodManager) hungerManager;
            if (nbt != null) {
                hungerManager.readNbt(nbt);
                nbt = null;
            }
            bloodManager.setOwner((PlayerEntity) (Object) this);
        }
    }

    @Override
    public void removeBloodManager() {
        hungerManager = new HungerManager();
        bloodManager = null;
    }
}
