package com.williambl.haema.mixin.vampire.ability.powers.damage_modification;

import com.williambl.haema.vampire.ability.powers.damage_modification.ExtendedDamageSource;
import net.minecraft.core.Holder;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.damagesource.DamageType;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(DamageSource.class)
public class DamageSourceMixin implements ExtendedDamageSource {
    private @Unique ItemStack weapon = ItemStack.EMPTY;

    @Inject(method = "<init>(Lnet/minecraft/core/Holder;Lnet/minecraft/world/entity/Entity;Lnet/minecraft/world/entity/Entity;Lnet/minecraft/world/phys/Vec3;)V", at = @At("RETURN"))
    private void haema$setWeapon(Holder<DamageType> holder, Entity directEntity, Entity causingEntity, Vec3 vec3, CallbackInfo ci) {
        if (directEntity instanceof LivingEntity attacker) {
            this.weapon = attacker.getMainHandItem();
        }
    }

    @Override
    public ItemStack weapon() {
        return this.weapon;
    }
}
