package com.williambl.haema.client.mixin.vampire.ability.powers.dash;

import com.llamalad7.mixinextras.sugar.Local;
import com.mojang.blaze3d.vertex.PoseStack;
import com.williambl.haema.client.vampire.ability.powers.dash.DashAbilityPowerClient;
import com.williambl.haema.client.vampire.ability.powers.dash.DashShimmerEffectManager;
import com.williambl.haema.vampire.ability.powers.dash.EntityChargingDashComponent;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(EntityRenderDispatcher.class)
public class EntityRenderDispatcherMixin {
    @Inject(method = "render", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/renderer/entity/EntityRenderer;render(Lnet/minecraft/world/entity/Entity;FFLcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/client/renderer/MultiBufferSource;I)V"))
    private <E extends Entity> void haema$renderAuras(E entity, double d, double e, double f, float g, float partialTick, PoseStack poseStack, MultiBufferSource multiBufferSource, int i, CallbackInfo ci, @Local EntityRenderer<? super E> entityRenderer) {
        Integer flashTicksRemaining = DashAbilityPowerClient.dashedEntities.get(entity);
        if (flashTicksRemaining != null || EntityChargingDashComponent.KEY.maybeGet(entity).filter(EntityChargingDashComponent::isChargingDash).isPresent()) {
            int alpha;
            if (flashTicksRemaining == null) {
                alpha = 255;
            } else {
                double flashFactor = (flashTicksRemaining - partialTick) / (float) DashAbilityPowerClient.DASH_FLASH_TICKS;
                alpha = (int) (255 * Math.pow(flashFactor, 3.0));
            }
            var auraBufferSource = DashShimmerEffectManager.buffers(multiBufferSource, 255, 255, 255, alpha);
            double time = entity.tickCount + partialTick;
            var offset = entity.getLookAngle().scale(-Math.abs(0.3 * (Math.sin(time / 20) + Math.sin(time * Math.PI))));
            poseStack.pushPose();
            poseStack.translate(offset.x, offset.y, offset.z);
            entityRenderer.render(entity, g, partialTick, poseStack, auraBufferSource, LightTexture.FULL_BRIGHT);
            poseStack.popPose();
        }
    }
}
