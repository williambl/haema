package com.williambl.haema.mixin;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.SunlightSicknessEffect;
import com.williambl.haema.VampireEntity;
import com.williambl.haema.VampireBloodManager;
import net.minecraft.block.BlockState;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
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
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(PlayerEntity.class)
public abstract class PlayerEntityMixin extends LivingEntity implements VampireEntity {

    @Shadow protected HungerManager hungerManager;
    protected VampireBloodManager bloodManager; // to avoid a load of casts

    protected PlayerEntityMixin(EntityType<? extends LivingEntity> entityType, World world) {
        super(entityType, world);
    }

    @Inject(method = "<init>", at = @At("TAIL"))
    void useCustomHungerManager(World world, BlockPos blockPos, GameProfile gameProfile, CallbackInfo ci) {
        hungerManager = new VampireBloodManager();
        bloodManager = (VampireBloodManager)hungerManager;
    }

    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/entity/LivingEntity;tick()V"))
    void damageInSunlight(CallbackInfo ci) {
        if (this.isInDaylight()) {
            bloodManager.removeBlood(0.01);
            this.addStatusEffect(new StatusEffectInstance(SunlightSicknessEffect.instance, 1, 1));
        }
    }

    protected boolean isInDaylight() {
        if (this.world.isDay() && !this.world.isClient) {
            float brightnessAtEyes = this.getBrightnessAtEyes();
            BlockPos blockPos = this.getVehicle() instanceof BoatEntity ?
                    (new BlockPos(this.getX(), (double)Math.round(this.getY()), this.getZ())).up()
                    : new BlockPos(this.getX(), (double)Math.round(this.getY()), this.getZ());

            return brightnessAtEyes > 0.5F
                    && this.random.nextFloat() * 30.0F < (brightnessAtEyes - 0.4F) * 2.0F
                    && this.world.isSkyVisible(blockPos);
        }

        return false;
    }
}
