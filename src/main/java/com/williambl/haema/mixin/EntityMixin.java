package com.williambl.haema.mixin;

import com.williambl.haema.ability.component.mist_form.MistFormAbilityComponent;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(Entity.class)
public class EntityMixin {
    @Inject(method = "isInvisible", at = @At("RETURN"), cancellable = true)
    void haema$makeMistFormInvisible(CallbackInfoReturnable<Boolean> cir) {
        cir.setReturnValue(cir.getReturnValueZ() || ((Object) this instanceof LivingEntity && MistFormAbilityComponent.Companion.getEntityKey().isProvidedBy(this) && MistFormAbilityComponent.Companion.getEntityKey().get(this).isInMistForm()));
    }
}
