package com.williambl.haema.mixin.hunters;

import com.williambl.haema.hunters.HaemaHunters;
import com.williambl.haema.hunters.VampireHunterSpawner;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.RandomSource;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.Level;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(LivingEntity.class)
public abstract class LivingEntityMixin extends Entity {
    public LivingEntityMixin(EntityType<?> entityType, Level level) {
        super(entityType, level);
    }

    @Shadow public abstract RandomSource getRandom();

    @Inject(method = "die", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/entity/LivingEntity;setPose(Lnet/minecraft/world/entity/Pose;)V"))
    private void haema$alertHuntersToDeath(DamageSource damageSource, CallbackInfo ci) {
        if (damageSource.is(HaemaHunters.HunterTags.ALERTS_HUNTERS) && this.level() instanceof ServerLevel level) {
            if (this.getRandom().nextDouble() < level.getGameRules().getRule(HaemaHunters.HunterGameRules.HUNTER_DEATH_NOTICE_CHANCE).get()) {
                /*if ((Object) this instanceof ServerPlayerEntity) { TODO uncomment when we have advancements
                    VampireHunterTriggerCriterion.INSTANCE.trigger((ServerPlayerEntity) (Object) this);
                }*/
                VampireHunterSpawner.trySpawnNear(level, this.getRandom(), this.blockPosition());
            }
        }
    }
}
