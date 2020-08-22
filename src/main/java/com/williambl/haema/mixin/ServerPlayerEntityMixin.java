package com.williambl.haema.mixin;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.Vampirable;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.network.ServerPlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ServerPlayerEntity.class)
public abstract class ServerPlayerEntityMixin extends PlayerEntity {

    public ServerPlayerEntityMixin(World world, BlockPos blockPos, float yaw, GameProfile gameProfile) {
        super(world, blockPos, yaw, gameProfile);
    }

    @Inject(method = "copyFrom", at = @At("TAIL"))
    void readVampireData(ServerPlayerEntity oldPlayer, boolean alive, CallbackInfo ci) {
        ((Vampirable)this).setVampire(((Vampirable)oldPlayer).isVampire());
        ((Vampirable)this).setPermanentVampire(((Vampirable)oldPlayer).isPermanentVampire());
    }

}
