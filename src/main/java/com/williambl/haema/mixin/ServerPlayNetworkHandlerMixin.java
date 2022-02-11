package com.williambl.haema.mixin;

import com.williambl.haema.ability.component.mist_form.MistFormAbilityComponent;
import net.minecraft.server.network.ServerPlayNetworkHandler;
import net.minecraft.util.Hand;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Coerce;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(targets = "net/minecraft/server/network/ServerPlayNetworkHandler$1")
public class ServerPlayNetworkHandlerMixin {
    @SuppressWarnings("ShadowTarget")
    @Shadow ServerPlayNetworkHandler field_28963; // ServerPlayNetworkHandler.this

    @SuppressWarnings("InvalidInjectorMethodSignature") // coerce
    @Inject(method = "processInteract", at = @At("HEAD"), cancellable = true)
    void mistFormCannotInteract(Hand hand, @Coerce Object action, CallbackInfo ci) {
        if (MistFormAbilityComponent.Companion.getEntityKey().get(field_28963.player).isInMistForm()) {
            ci.cancel();
        }
    }

    @Inject(method = "attack", at = @At("HEAD"), cancellable = true)
    void mistFormCannotAttack(CallbackInfo ci) {
        if (MistFormAbilityComponent.Companion.getEntityKey().get(field_28963.player).isInMistForm()) {
            ci.cancel();
        }
    }
}
