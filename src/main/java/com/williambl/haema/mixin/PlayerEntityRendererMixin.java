package com.williambl.haema.mixin;

import com.williambl.haema.abilities.bat.BatFormable;
import com.williambl.haema.client.HaemaClientKt;
import net.minecraft.client.model.ModelPart;
import net.minecraft.client.network.AbstractClientPlayerEntity;
import net.minecraft.client.render.VertexConsumerProvider;
import net.minecraft.client.render.entity.EntityRenderDispatcher;
import net.minecraft.client.render.entity.LivingEntityRenderer;
import net.minecraft.client.render.entity.PlayerEntityRenderer;
import net.minecraft.client.render.entity.model.PlayerEntityModel;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.passive.BatEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(PlayerEntityRenderer.class)
public abstract class PlayerEntityRendererMixin
        extends LivingEntityRenderer<AbstractClientPlayerEntity, PlayerEntityModel<AbstractClientPlayerEntity>> {
    public PlayerEntityRendererMixin(EntityRenderDispatcher dispatcher, PlayerEntityModel<AbstractClientPlayerEntity> model, float shadowRadius) {
        super(dispatcher, model, shadowRadius);
    }

    @Inject(method = "renderArm", at = @At("HEAD"), cancellable = true)
    private void batsDontHaveArms(MatrixStack matrices, VertexConsumerProvider vertexConsumers, int light, AbstractClientPlayerEntity player, ModelPart arm, ModelPart sleeve, CallbackInfo ci) {
        if (((BatFormable)player).isBat()) ci.cancel();
    }

    @Inject(method = "render", at=@At("HEAD"), cancellable = true)
    private void makeBatFormVampiresLookLikeBats(AbstractClientPlayerEntity abstractClientPlayerEntity, float yaw, float tickDelta, MatrixStack matrixStack, VertexConsumerProvider vertexConsumerProvider, int light, CallbackInfo ci) {
        if (((BatFormable)abstractClientPlayerEntity).isBat()) {
            BatEntity bat = HaemaClientKt.getBatEntity(abstractClientPlayerEntity);
            dispatcher.getRenderer(bat).render(bat, yaw, tickDelta, matrixStack, vertexConsumerProvider, light);
            ci.cancel();
        }
    }
}
