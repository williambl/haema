package com.williambl.haema.mixin;

import com.google.common.base.Suppliers;
import com.williambl.haema.damagesource.DamageSourceModule;
import net.minecraft.enchantment.EnchantmentHelper;
import net.minecraft.enchantment.Enchantments;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.registry.tag.TagKey;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.function.Supplier;

import static com.williambl.haema.HaemaKt.id;

@Mixin(DamageSource.class)
public class DamageSourceMixin {
    private static final Supplier<TagKey<Item>> vampireEffectiveWeaponsTag = Suppliers.memoize(() -> TagKey.of(RegistryKeys.ITEM, id("vampire_weapons")));

    @Inject(method = "player", at = @At("RETURN"))
    private static void addVampireEffectiveSources(PlayerEntity attacker, CallbackInfoReturnable<DamageSource> cir) {
        setEffective(attacker, cir);
    }

    @Inject(method = "mob", at = @At("TAIL"))
    private static void addVampireEffectiveSourcesMob(LivingEntity attacker, CallbackInfoReturnable<DamageSource> cir) {
        setEffective(attacker, cir);
    }

    @Unique
    private static void setEffective(LivingEntity attacker, CallbackInfoReturnable<DamageSource> cir) {
        DamageSource source = cir.getReturnValue();
        ItemStack stack = attacker.getMainHandStack();
        int smiteLevel = EnchantmentHelper.getLevel(Enchantments.SMITE, stack);
        if (stack.isIn(vampireEffectiveWeaponsTag.get()) || smiteLevel >= 1)
            DamageSourceModule.INSTANCE.setEffectiveAgainstVampires(source, Math.max(1.25f, smiteLevel * 1.25f));
    }
}
