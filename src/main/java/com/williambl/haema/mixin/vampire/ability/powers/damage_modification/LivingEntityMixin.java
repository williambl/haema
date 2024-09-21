package com.williambl.haema.mixin.vampire.ability.powers.damage_modification;

import com.williambl.haema.vampire.ability.powers.damage_modification.CheatDeathCallback;
import com.williambl.haema.vampire.ability.powers.damage_modification.DamageModificationCallback;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.LivingEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.ModifyVariable;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(LivingEntity.class)
public class LivingEntityMixin {
    @Shadow protected float lastHurt;

    @ModifyVariable(method = "getDamageAfterMagicAbsorb", at = @At(value = "RETURN", ordinal = 3), argsOnly = true)
    private float haema$modifyDamage(float amount, DamageSource source) {
        return DamageModificationCallback.EVENT.invoker().modifyDamage(source, amount, (LivingEntity) (Object) this);
    }

    @Inject(method = "checkTotemDeathProtection", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/InteractionHand;values()[Lnet/minecraft/world/InteractionHand;"), cancellable = true)
    private void haema$cheatDeath(DamageSource damageSource, CallbackInfoReturnable<Boolean> cir) {
        if (CheatDeathCallback.EVENT.invoker().shouldSurvive(damageSource, this.lastHurt, (LivingEntity) (Object) this)) {
            cir.setReturnValue(true);
        }
    }
}
