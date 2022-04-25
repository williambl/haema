package com.williambl.haema.mixin;

import com.williambl.haema.damagesource.DamageSourceModule;
import net.minecraft.enchantment.EnchantmentHelper;
import net.minecraft.enchantment.Enchantments;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.tag.TagKey;
import net.minecraft.util.registry.Registry;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static com.williambl.haema.HaemaKt.id;

@Mixin(DamageSource.class)
public class DamageSourceMixin {
    private static final TagKey<Item> vampireEffectiveWeaponsTag = TagKey.of(Registry.ITEM_KEY, id("vampire_weapons"));

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
        if (stack.isIn(vampireEffectiveWeaponsTag) || smiteLevel >= 1)
            DamageSourceModule.INSTANCE.setEffectiveAgainstVampires(source, Math.max(1.25f, smiteLevel * 1.25f));
    }
}
