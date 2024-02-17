package com.williambl.haema.mixin.vampire.ability.powers.sleep;

import com.llamalad7.mixinextras.injector.wrapoperation.Operation;
import com.llamalad7.mixinextras.injector.wrapoperation.WrapOperation;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.sleep.SleepInDayAbilityPower;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import org.checkerframework.checker.units.qual.A;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Slice;

import java.util.List;

@Mixin(ServerLevel.class)
public abstract class ServerLevelMixin {
    @Shadow public abstract List<ServerPlayer> players();

    @WrapOperation(method = "tick", slice = @Slice(
            from = @At(value = "INVOKE", target = "Lnet/minecraft/server/players/SleepStatus;areEnoughSleeping(I)Z"),
            to = @At(value = "INVOKE", target = "Lnet/minecraft/server/level/ServerLevel;wakeUpAllPlayers()V")),
            at = @At(value = "INVOKE", target = "Lnet/minecraft/server/level/ServerLevel;setDayTime(J)V"))
    private void haema$skipToNight(ServerLevel level, long time, Operation<Void> original) {
        boolean areAllDaySleepers = this.players().stream()
                .filter(Player::isSleepingLongEnough)
                .noneMatch(p -> VampireAbilitiesComponent.KEY.get(p).getEnabledPowersOfClass(SleepInDayAbilityPower.class).isEmpty());
        if (areAllDaySleepers) {
            original.call(level, time + 13000L);
        }
    }
}
