package com.williambl.haema.mixin;

import com.williambl.haema.HaemaKt;
import com.williambl.haema.damagesource.DamageSourceExtensionsKt;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.damage.EntityDamageSource;
import net.minecraft.entity.player.PlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(DamageSource.class)
public class DamageSourceMixin {

    @Inject(method = "player", at = @At("HEAD"), cancellable = true)
    private static void addVampireEffectiveSources(PlayerEntity attacker, CallbackInfoReturnable<DamageSource> cir) {
        DamageSource source = new EntityDamageSource("player", attacker);
        if (attacker.getMainHandStack().getItem().isIn(HaemaKt.getVampireEffectiveWeaponsTag()))
            DamageSourceExtensionsKt.setEffectiveAgainstVampires(source);
        cir.setReturnValue(source);
    }
}
