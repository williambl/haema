package com.williambl.haema.mixin;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.VampireEntity;
import com.williambl.haema.VampireBloodManager;
import net.minecraft.block.BlockState;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.HungerManager;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(PlayerEntity.class)
public abstract class PlayerEntityMixin extends LivingEntity implements VampireEntity {

    @Shadow protected HungerManager hungerManager;

    protected PlayerEntityMixin(EntityType<? extends LivingEntity> entityType, World world) {
        super(entityType, world);
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    void useCustomHungerManager(World world, BlockPos blockPos, GameProfile gameProfile, CallbackInfo ci) {
        hungerManager = new VampireBloodManager();
    }
}
