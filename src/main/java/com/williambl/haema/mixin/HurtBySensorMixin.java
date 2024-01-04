package com.williambl.haema.mixin;

import com.williambl.haema.damagetype.DamageTypeModule;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.ai.brain.sensor.HurtBySensor;
import net.minecraft.entity.damage.DamageSource;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(HurtBySensor.class)
public class HurtBySensorMixin {

    @Redirect(method = "sense", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;getRecentDamageSource()Lnet/minecraft/entity/damage/DamageSource;"))
    DamageSource ignoreBloodLossIfAsleep(LivingEntity livingEntity) {
        DamageSource result = livingEntity.getRecentDamageSource();
        if (livingEntity.isSleeping() && result != null && result.isOf(DamageTypeModule.INCOMPATIBLE_BLOOD))
            return null;
        return result;
    }
}
