package com.williambl.haema.compat.mixin.bewitchment;

import com.williambl.haema.Vampirable;
import moriyashiine.bewitchment.common.registry.BWDamageSources;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(BWDamageSources.class)
public class BWDamageSourcesMixin {
    @Inject(method = "handleVampireDamage", at=@At("HEAD"), cancellable = true, remap = false)
    private static void haema$handleVampireDamge(LivingEntity entity, DamageSource source, float amount, CallbackInfoReturnable<Float> cir) {
        if ((entity instanceof Vampirable) && ((Vampirable) entity).isVampire()) {
            cir.setReturnValue(amount);
        }
    }
}
