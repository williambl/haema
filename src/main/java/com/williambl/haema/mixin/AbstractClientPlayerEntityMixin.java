package com.williambl.haema.mixin;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.util.EntityAttributeInstanceExtensionsKt;
import net.minecraft.client.network.AbstractClientPlayerEntity;
import net.minecraft.entity.attribute.EntityAttribute;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

import java.util.UUID;

@Mixin(AbstractClientPlayerEntity.class)
public abstract class AbstractClientPlayerEntityMixin extends PlayerEntity {
    private static final UUID movementSpeedAttributeUUID = UUID.fromString("7a47b1b8-16a5-4877-905a-07ffd5d2189b");

    public AbstractClientPlayerEntityMixin(World world, BlockPos pos, float yaw, GameProfile profile) {
        super(world, pos, yaw, profile);
    }

    @Redirect(method = "getSpeed", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/network/AbstractClientPlayerEntity;getAttributeValue(Lnet/minecraft/entity/attribute/EntityAttribute;)D"))
    public double disregardVampireSpeed(AbstractClientPlayerEntity abstractClientPlayerEntity, EntityAttribute attribute) {
        return EntityAttributeInstanceExtensionsKt.computeValueWithout(abstractClientPlayerEntity.getAttributeInstance(attribute), movementSpeedAttributeUUID);
    }
}
