package com.williambl.haema.mixin;

import com.mojang.blaze3d.systems.RenderSystem;
import com.williambl.haema.Vampirable;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gui.DrawableHelper;
import net.minecraft.client.gui.hud.InGameHud;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.Identifier;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Redirect;
import org.spongepowered.asm.mixin.injection.Slice;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static com.williambl.haema.HaemaKt.id;

@Mixin(InGameHud.class)
public class InGameHudMixin extends DrawableHelper {
    @Shadow @Final private MinecraftClient client;

    private static final Identifier EMPTY_BLOOD_ICON = id("textures/gui/blood_empty.png");
    private static final Identifier FULL_BLOOD_ICON = id("textures/gui/blood_full.png");
    private static final Identifier HALF_BLOOD_ICON = id("textures/gui/blood_half.png");

    @Redirect(
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
                    target = "Lnet/minecraft/client/gui/hud/InGameHud;drawTexture(Lnet/minecraft/client/util/math/MatrixStack;IIIIII)V"
            )
    )
    void showVampireBloodIcons(InGameHud inGameHud, MatrixStack matrices, int x, int y, int u, int v, int width, int height) {
        PlayerEntity player = client.player;
        if (player instanceof Vampirable && ((Vampirable)player).isVampire()) {
            drawTexture(matrices, x, y, 0, 0, width, height, 9, 9);
            RenderSystem.setShaderTexture(0, GUI_ICONS_TEXTURE);
        } else {
            inGameHud.drawTexture(matrices, x, y, u, v, width, height);
        }
    }

    @Inject(
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
                    target = "Lnet/minecraft/client/gui/hud/InGameHud;drawTexture(Lnet/minecraft/client/util/math/MatrixStack;IIIIII)V",
                    ordinal = 0
            )
    )
    void switchToEmptyBloodIcon(MatrixStack matrixStack, CallbackInfo ci) {
        PlayerEntity player = client.player;
        if (player instanceof Vampirable && ((Vampirable)player).isVampire()) {
            RenderSystem.setShaderTexture(0, EMPTY_BLOOD_ICON);
        }
    }
    @Inject(
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
                    target = "Lnet/minecraft/client/gui/hud/InGameHud;drawTexture(Lnet/minecraft/client/util/math/MatrixStack;IIIIII)V",
                    ordinal = 2
            )
    )
    void switchToHalfBloodIcon(MatrixStack matrixStack, CallbackInfo ci) {
        PlayerEntity player = client.player;
        if (player instanceof Vampirable && ((Vampirable)player).isVampire()) {
            RenderSystem.setShaderTexture(0, HALF_BLOOD_ICON);
        }
    }

    @Inject(
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
                    target = "Lnet/minecraft/client/gui/hud/InGameHud;drawTexture(Lnet/minecraft/client/util/math/MatrixStack;IIIIII)V",
                    ordinal = 1
            )
    )
    void switchToFullBloodIcon(MatrixStack matrixStack, CallbackInfo ci) {
        PlayerEntity player = client.player;
        if (player instanceof Vampirable && ((Vampirable)player).isVampire()) {
            RenderSystem.setShaderTexture(0, FULL_BLOOD_ICON);
        }
    }
}
