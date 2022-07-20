package com.williambl.haema.mixin.client;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.util.EntityAttributeInstanceExtensionsKt;
import net.minecraft.client.network.AbstractClientPlayerEntity;
import net.minecraft.entity.attribute.EntityAttribute;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.network.encryption.PlayerPublicKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import java.util.UUID;

@Mixin(AbstractClientPlayerEntity.class)
public abstract class AbstractClientPlayerEntityMixin extends PlayerEntity {
    private static final UUID movementSpeedAttributeUUID = UUID.fromString("7a47b1b8-16a5-4877-905a-07ffd5d2189b");

    public AbstractClientPlayerEntityMixin(World world, BlockPos pos, float yaw, GameProfile gameProfile, @Nullable PlayerPublicKey publicKey) {
        super(world, pos, yaw, gameProfile, publicKey);
    }

    @Redirect(method = "getFovMultiplier", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/network/AbstractClientPlayerEntity;getAttributeValue(Lnet/minecraft/entity/attribute/EntityAttribute;)D"))
    public double disregardVampireSpeed(AbstractClientPlayerEntity abstractClientPlayerEntity, EntityAttribute attribute) {
        return EntityAttributeInstanceExtensionsKt.computeValueWithout(abstractClientPlayerEntity.getAttributeInstance(attribute), movementSpeedAttributeUUID);
    }
}
