package com.williambl.haema.mixin.client;

import com.williambl.haema.VampirableKt;
import com.williambl.haema.ability.AbilityModule;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.LightmapTextureManager;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.ModifyVariable;

@Mixin(LightmapTextureManager.class)
public class LightmapTextureManagerMixin {
    @ModifyVariable(
            method = "update",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/util/math/Vec3f;<init>(FFF)V", ordinal = 0),
            ordinal = 3
    )
    private float haema$changeLightForVampireVision(float value) {
        var player = MinecraftClient.getInstance().player;
        if (player != null && VampirableKt.isVampire(player) && VampirableKt.getAbilityLevel(player, AbilityModule.INSTANCE.getVISION()) > 0) {
            return Math.max(value, 0.1f);
        }
        return value;
    }
}
