package com.williambl.haema.client.mixin.vampire.ability.powers.vision;

import com.llamalad7.mixinextras.sugar.Local;
import com.mojang.blaze3d.vertex.PoseStack;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.client.vampire.ability.powers.vision.AuraVertexConsumerProvider;
import com.williambl.haema.client.vampire.ability.powers.vision.GlowEffectManager;
import com.williambl.haema.client.vampire.ability.powers.vision.VampireVisionFx;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;

@Mixin(EntityRenderDispatcher.class)
public class EntityRenderDispatcherMixin {
    @Inject(method = "render", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/renderer/entity/EntityRenderer;render(Lnet/minecraft/world/entity/Entity;FFLcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/MultiBufferSource;I)V"))
    private <E extends Entity> void haema$renderAuras(E entity, double d, double e, double f, float g, float h, PoseStack poseStack, MultiBufferSource multiBufferSource, int i, CallbackInfo ci, @Local EntityRenderer<? super E> entityRenderer) {
        if (VampireVisionFx.INSTANCE.isRenderingThisTick()) {
            BloodApi.getBloodQuality(entity).ifPresent(q -> {
                var auraBufferSource = new AuraVertexConsumerProvider(multiBufferSource, 255, 0, q == BloodQuality.PERFECT ? 255 : 0, (int) Math.min(255, q.multiplier * 255));
                entityRenderer.render(entity, g, h, poseStack, auraBufferSource, LightTexture.FULL_BRIGHT);
            });
        }
    }
}
