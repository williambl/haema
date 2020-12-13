package com.williambl.haema.mixin;

import net.minecraft.server.world.ServerWorld;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.Slice;

@Mixin(ServerWorld.class)
public abstract class ServerWorldMixin {
    @Redirect(
            method = "tick",
            slice = @Slice(
                    from = @At(value = "INVOKE", target = "Lnet/minecraft/world/GameRules;getBoolean(Lnet/minecraft/world/GameRules$Key;)Z", ordinal = 1),
                    to = @At(value = "INVOKE", target = "Lnet/minecraft/server/world/ServerWorld;setTimeOfDay(J)V")
            ),
            at = @At(value = "INVOKE", target = "Lnet/minecraft/server/world/ServerWorld;setTimeOfDay(J)V")
    )
    void modifySleepLengthInDay(ServerWorld serverWorld, long timeOfDay) {
        if (!serverWorld.isDay()) {
            serverWorld.setTimeOfDay(timeOfDay);
        } else {
            serverWorld.setTimeOfDay(serverWorld.getTimeOfDay() + (13000L - (serverWorld.getTimeOfDay() % 24000L)));
        }
    }
}
