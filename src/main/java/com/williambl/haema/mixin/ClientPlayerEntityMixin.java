package com.williambl.haema.mixin;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.SunlightSicknessEffect;
import com.williambl.haema.VampireBloodManager;
import com.williambl.haema.VampireEntity;
import com.williambl.haema.client.HaemaClientKt;
import net.minecraft.client.network.AbstractClientPlayerEntity;
import net.minecraft.client.network.ClientPlayerEntity;
import net.minecraft.client.world.ClientWorld;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.damage.DamageSource;
import net.minecraft.entity.effect.StatusEffectInstance;
import net.minecraft.entity.player.HungerManager;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.vehicle.BoatEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ClientPlayerEntity.class)
public abstract class ClientPlayerEntityMixin extends AbstractClientPlayerEntity implements VampireEntity {

    public ClientPlayerEntityMixin(ClientWorld world, GameProfile profile) {
        super(world, profile);
    }

    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/network/AbstractClientPlayerEntity;tick()V"))
    void useShaders(CallbackInfo ci) {
        HaemaClientKt.getVAMPIRE_SHADER().setUniformValue("Saturation", 0.8f * (float) ((VampireBloodManager)this.hungerManager).getBloodLevel()/20.0f);
        HaemaClientKt.getVAMPIRE_SHADER().setUniformValue("RedMatrix",  Math.max(1.3f, 2.3f - (this.world.getTime() - ((VampireBloodManager)this.hungerManager).getLastFed())/(float)VampireBloodManager.FEED_COOLDOWN), 0f, 0f);
    }
}
