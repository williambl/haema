package com.williambl.haema.mixin.client;

import com.llamalad7.mixinextras.injector.WrapWithCondition;
import com.williambl.haema.VampirableKt;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gui.DrawContext;
import net.minecraft.client.gui.hud.InGameHud;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.Identifier;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Slice;

import static com.williambl.haema.HaemaKt.id;

@Mixin(InGameHud.class)
public class InGameHudMixin {
    @Shadow @Final private MinecraftClient client;

    @Unique
    private static final Identifier EMPTY_BLOOD_ICON = id("textures/gui/blood_empty.png");

    @Unique
    private static final Identifier FULL_BLOOD_ICON = id("textures/gui/blood_full.png");

    @Unique
    private static final Identifier HALF_BLOOD_ICON = id("textures/gui/blood_half.png");

    @Unique
    private boolean showVampireBloodIcon(DrawContext context, Identifier texture, int x, int y, int width, int height) {
        PlayerEntity player = client.player;
        if (VampirableKt.isVampire(player)) {
            context.drawTexture(texture, x, y, 0, 0, width, height, 9, 9);
            return false;
        } else {
            return true;
        }
    }

    @WrapWithCondition(
            method = "renderStatusBars",
            slice = @Slice(
                    from = @At(
                            value = "INVOKE_ASSIGN",
                            target = "Lnet/minecraft/client/gui/hud/InGameHud;getHeartCount(Lnet/minecraft/entity/LivingEntity;)I"
                    ),
                    to = @At(
                            value = "INVOKE",
                            target = "Lnet/minecraft/entity/player/PlayerEntity;getAir()I"
                    )
            ),
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/client/gui/DrawContext;drawTexture(Lnet/minecraft/util/Identifier;IIIIII)V",
                    ordinal = 0
            )
    )
    boolean showVampireEmptyBloodIcon(DrawContext context, Identifier texture, int x, int y, int u, int v, int width, int height) {
        return showVampireBloodIcon(context, EMPTY_BLOOD_ICON, x, y, width, height);
    }

    @WrapWithCondition(
            method = "renderStatusBars",
            slice = @Slice(
                    from = @At(
                            value = "INVOKE_ASSIGN",
                            target = "Lnet/minecraft/client/gui/hud/InGameHud;getHeartCount(Lnet/minecraft/entity/LivingEntity;)I"
                    ),
                    to = @At(
                            value = "INVOKE",
                            target = "Lnet/minecraft/entity/player/PlayerEntity;getAir()I"
                    )
            ),
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/client/gui/DrawContext;drawTexture(Lnet/minecraft/util/Identifier;IIIIII)V",
                    ordinal = 1
            )
    )
    boolean showVampireHalfBloodIcon(DrawContext context, Identifier texture, int x, int y, int u, int v, int width, int height) {
        return showVampireBloodIcon(context, HALF_BLOOD_ICON, x, y, width, height);
    }

    @WrapWithCondition(
            method = "renderStatusBars",
            slice = @Slice(
                    from = @At(
                            value = "INVOKE_ASSIGN",
                            target = "Lnet/minecraft/client/gui/hud/InGameHud;getHeartCount(Lnet/minecraft/entity/LivingEntity;)I"
                    ),
                    to = @At(
                            value = "INVOKE",
                            target = "Lnet/minecraft/entity/player/PlayerEntity;getAir()I"
                    )
            ),
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/client/gui/DrawContext;drawTexture(Lnet/minecraft/util/Identifier;IIIIII)V",
                    ordinal = 2
            )
    )
    boolean showVampireFullBloodIcon(DrawContext context, Identifier texture, int x, int y, int u, int v, int width, int height) {
        return showVampireBloodIcon(context, FULL_BLOOD_ICON, x, y, width, height);
    }
}
