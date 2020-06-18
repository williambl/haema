package com.williambl.haema.mixin;

import com.williambl.haema.HaemaKt;
import com.williambl.haema.damagesource.DamageSourceExtensionsKt;
import net.minecraft.enchantment.EnchantmentHelper;
import net.minecraft.enchantment.Enchantments;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.damage.EntityDamageSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(DamageSource.class)
public class DamageSourceMixin {

    @Inject(method = "player", at = @At("HEAD"), cancellable = true)
    private static void addVampireEffectiveSources(PlayerEntity attacker, CallbackInfoReturnable<DamageSource> cir) {
        DamageSource source = new EntityDamageSource("player", attacker);
        ItemStack stack = attacker.getMainHandStack();
        if (stack.getItem().isIn(HaemaKt.getVampireEffectiveWeaponsTag()) || EnchantmentHelper.getLevel(Enchantments.SMITE, stack) >= 1)
            DamageSourceExtensionsKt.setEffectiveAgainstVampires(source);
        cir.setReturnValue(source);
    }
}
