package com.williambl.haema.compat.mixin.bewitchment;

import com.williambl.haema.Vampirable;
import com.williambl.haema.abilities.VampireAbility;
import com.williambl.haema.hunter.VampireHunterEntity;
import moriyashiine.bewitchment.common.entity.living.VampireEntity;
import moriyashiine.bewitchment.common.entity.living.util.BWHostileEntity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.ai.goal.ActiveTargetGoal;
import net.minecraft.entity.mob.HostileEntity;
import net.minecraft.util.Identifier;
import net.minecraft.world.World;
import org.jetbrains.annotations.NotNull;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(VampireEntity.class)
public abstract class VampireEntityMixin extends BWHostileEntity implements Vampirable {
    protected VampireEntityMixin(EntityType<? extends HostileEntity> entityType, World world) {
        super(entityType, world);
    }

    @Inject(method = "initGoals", at = @At("TAIL"))
    void haema$initGoals(CallbackInfo ci) {
        targetSelector.add(1, new ActiveTargetGoal<>(this, LivingEntity.class, 10, true, false,
                (entity) -> entity instanceof VampireHunterEntity));
    }

    @Override
    public boolean isVampire() {
        return true;
    }

    @Override
    public void setVampire(boolean value) {}

    @Override
    public boolean isPermanentVampire() {
        return true;
    }

    @Override
    public void setPermanentVampire(boolean value) {}

    @Override
    public boolean isKilled() {
        return isDead();
    }

    @Override
    public void setKilled(boolean value) {}

    @Override
    public int getAbilityLevel(@NotNull VampireAbility ability) {
        return 0;
    }

    @Override
    public void setAbilityLevel(@NotNull VampireAbility ability, int level) {}

    @Override
    public boolean hasUsedRitual(@NotNull Identifier identifier) {
        return false;
    }

    @Override
    public void setHasUsedRitual(@NotNull Identifier identifier, boolean has) {}

    @Override
    public void checkBloodManager() {}

    @Override
    public void removeBloodManager() {}
}
