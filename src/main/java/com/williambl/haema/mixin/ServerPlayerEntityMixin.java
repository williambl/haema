package com.williambl.haema.mixin;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.Vampirable;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(ServerPlayerEntity.class)
public abstract class ServerPlayerEntityMixin extends PlayerEntity {
    public ServerPlayerEntityMixin(World world, BlockPos pos, float yaw, GameProfile profile) {
        super(world, pos, yaw, profile);
    }

    @Redirect(method = "trySleep(Lnet/minecraft/util/math/BlockPos;)Lcom/mojang/datafixers/util/Either;", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/World;isDay()Z"))
    boolean allowVampiresToSleepInDay(World world) {
        return world.isDay() && !(this instanceof Vampirable && ((Vampirable) this).isVampire());
    }
}
