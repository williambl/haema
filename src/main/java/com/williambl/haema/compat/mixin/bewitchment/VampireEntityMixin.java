package com.williambl.haema.compat.mixin.bewitchment;

import com.williambl.haema.VampirableKt;
import com.williambl.haema.hunter.VampireHunterEntity;
import moriyashiine.bewitchment.common.entity.living.VampireEntity;
import moriyashiine.bewitchment.common.entity.living.util.BWHostileEntity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.ai.goal.ActiveTargetGoal;
import net.minecraft.entity.mob.HostileEntity;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

//TODO: improve behaviour, make use of blood
@Mixin(VampireEntity.class)
public abstract class VampireEntityMixin extends BWHostileEntity {
    protected VampireEntityMixin(EntityType<? extends HostileEntity> entityType, World world) {
        super(entityType, world);
    }

    @Inject(method = "initGoals", at = @At("TAIL"))
    void haema$initGoals(CallbackInfo ci) {
        targetSelector.add(1, new ActiveTargetGoal<>(this, LivingEntity.class, 10, true, false,
                (entity) -> entity instanceof VampireHunterEntity));
    }

    @Inject(method = "tick", at = @At("HEAD"))
    void haema$makeVampire(CallbackInfo ci) {
        if (!VampirableKt.isVampire(this)) {
            VampirableKt.convert(this);
        }
    }
}
