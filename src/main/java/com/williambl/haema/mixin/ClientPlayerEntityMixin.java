package com.williambl.haema.mixin;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.Vampirable;
import com.williambl.haema.VampireBloodManager;
import com.williambl.haema.client.ClientVampire;
import com.williambl.haema.client.HaemaClientKt;
import com.williambl.haema.util.RaytraceUtilKt;
import io.netty.buffer.Unpooled;
import net.fabricmc.fabric.api.network.ClientSidePacketRegistry;
import net.minecraft.client.network.AbstractClientPlayerEntity;
import net.minecraft.client.network.ClientPlayerEntity;
import net.minecraft.client.world.ClientWorld;
import net.minecraft.network.PacketByteBuf;
import net.minecraft.particle.DustParticleEffect;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.Vec3d;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ClientPlayerEntity.class)
public abstract class ClientPlayerEntityMixin extends AbstractClientPlayerEntity implements ClientVampire {

    private int pressedTicks = 0;
    private long lastDashed = -24000;

    public ClientPlayerEntityMixin(ClientWorld world, GameProfile profile) {
        super(world, profile);
    }

    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/network/AbstractClientPlayerEntity;tick()V"))
    void useShaders(CallbackInfo ci) {
        if (((Vampirable) this).isVampire() && this.hungerManager instanceof VampireBloodManager) {
            float bloodLevel = ((float) ((VampireBloodManager)this.hungerManager).getBloodLevel()) / 20.0f;
            HaemaClientKt.setSaturation(0.8f * bloodLevel);
            HaemaClientKt.setColorScale(bloodLevel + 1.4f);

            HaemaClientKt.setRedAmount(Math.max(1.3f, 2.3f - (this.world.getTime() - ((VampireBloodManager) this.hungerManager).getLastFed()) / (float) VampireBloodManager.Companion.getFeedCooldown(world)));

            if (pressedTicks > 0 && !(HaemaClientKt.getDASH_KEY().isPressed()) && canDash()) {
                PacketByteBuf buf = new PacketByteBuf(Unpooled.buffer());
                ClientSidePacketRegistry.INSTANCE.sendToServer(new Identifier("haema:dash"), buf);
                lastDashed = world.getTime();
            } else if (HaemaClientKt.getDASH_KEY().isPressed() && canDash()) {
                Vec3d target = RaytraceUtilKt.raytraceForDash(this);
                if (target != null) for (int i = 0; i < 10; i++) {
                    world.addParticle(new DustParticleEffect(0, 0, 0, 1), target.x - 0.5 + random.nextDouble(), target.y + random.nextDouble() * 2, target.z - 0.5 + random.nextDouble(), 0.0, 0.5, 0.0);
                }
            }
            pressedTicks = HaemaClientKt.getDASH_KEY().isPressed() ? pressedTicks + 1 : 0;


            long timeSinceDash = world.getTime() - lastDashed;

            float distortAmount = HaemaClientKt.getDistortAmount();
            if (pressedTicks > 0 && canDash()) {
                HaemaClientKt.setDistortAmount(Math.max(HaemaClientKt.getDistortAmount() - 0.05f, -0.2f));
            } else if (timeSinceDash <= 8) {
                if (timeSinceDash == 0)
                    HaemaClientKt.setDistortAmount(-1.4f);
                else
                    HaemaClientKt.setDistortAmount(-0.25f + 0.25f*(float) Math.log(timeSinceDash/3f));
            } else if (distortAmount != 0) {
                if (Math.abs(distortAmount) < 0.1) {
                    HaemaClientKt.setDistortAmount(0f);
                } else {
                    HaemaClientKt.setDistortAmount(distortAmount - Math.copySign(0.1f, distortAmount));
                }
            }
        }
    }

    @Override
    public boolean canDash() {
        return world.getTime() > lastDashed+HaemaClientKt.getDashCooldownValue() && hungerManager instanceof VampireBloodManager && (((VampireBloodManager)hungerManager).getBloodLevel() > 18 || abilities.creativeMode);
    }
}
