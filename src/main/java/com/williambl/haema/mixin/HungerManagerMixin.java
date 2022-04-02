package com.williambl.haema.mixin;

import com.williambl.haema.VampirableKt;
import net.minecraft.entity.player.HungerManager;
import net.minecraft.entity.player.PlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(HungerManager.class)
public class HungerManagerMixin {
    @Unique private boolean isVampire;
    @Unique private PlayerEntity player;

    @Inject(method = "getFoodLevel", at = @At("HEAD"), cancellable = true)
    void haema$getBloodLevel(CallbackInfoReturnable<Integer> cir) {
        if (this.isVampire) {
            cir.setReturnValue((int) Math.floor(VampirableKt.getVampireComponent(player).getBlood()));
        }
    }

    @Inject(method = "getSaturationLevel", at = @At("HEAD"), cancellable = true)
    void haema$fakeSaturationLevel(CallbackInfoReturnable<Float> cir) {
        if (this.isVampire) {
            cir.setReturnValue(0f);
        }
    }

    @Inject(method = "update", at = @At("HEAD"), cancellable = true)
    void haema$setVampire(PlayerEntity player, CallbackInfo ci) {
        this.isVampire = VampirableKt.isVampire(player);
        this.player = player;

        if (this.isVampire) {
            ci.cancel();
        }
    }
}
