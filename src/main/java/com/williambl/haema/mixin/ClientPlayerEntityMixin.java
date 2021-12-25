package com.williambl.haema.mixin;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.VampirableKt;
import com.williambl.haema.ability.AbilityModule;
import com.williambl.haema.client.ClientVampire;
import com.williambl.haema.client.HaemaClient;
import com.williambl.haema.component.VampirePlayerComponent;
import com.williambl.haema.util.RaytraceUtilKt;
import io.netty.buffer.Unpooled;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.minecraft.client.network.AbstractClientPlayerEntity;
import net.minecraft.client.network.ClientPlayerEntity;
import net.minecraft.client.world.ClientWorld;
import net.minecraft.network.PacketByteBuf;
import net.minecraft.particle.DustParticleEffect;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.math.Vec3f;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import static com.williambl.haema.HaemaKt.id;

@Mixin(ClientPlayerEntity.class)
public abstract class ClientPlayerEntityMixin extends AbstractClientPlayerEntity implements ClientVampire {

    private int pressedTicks = 0;
    private long lastDashed = -24000;

    public ClientPlayerEntityMixin(ClientWorld world, GameProfile profile) {
        super(world, profile);
    }

    //TODO: move all this to a clienttick event if possible??
    @Inject(method = "tick", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/network/AbstractClientPlayerEntity;tick()V"))
    void useShaders(CallbackInfo ci) {
        if (VampirableKt.isVampire(this)) {
            float bloodLevel = ((float) VampirableKt.getVampireComponent(this).getBlood()) / 20.0f;
            HaemaClient.INSTANCE.setSaturation(0.8f * bloodLevel);
            HaemaClient.INSTANCE.setBrightnessAdjust(bloodLevel/4f+0.05f);

            HaemaClient.INSTANCE.setRedAmount(Math.max(1.3f, 2.3f - (this.world.getTime() - (VampirableKt.getVampireComponent(this).getLastFed())) / (float) VampirePlayerComponent.Companion.getFeedCooldown(world)));

            if (pressedTicks > 0 && !(HaemaClient.INSTANCE.getDASH_KEY().isPressed()) && canDash()) {
                PacketByteBuf buf = new PacketByteBuf(Unpooled.buffer());
                ClientPlayNetworking.send(id("dash"), buf);
                lastDashed = world.getTime();
            } else if (HaemaClient.INSTANCE.getDASH_KEY().isPressed() && canDash()) {
                Vec3d target = RaytraceUtilKt.raytraceForDash(this);
                if (target != null) for (int i = 0; i < 10; i++) {
                    world.addParticle(new DustParticleEffect(new Vec3f(0, 0, 0), 1), target.x - 0.5 + random.nextDouble(), target.y + random.nextDouble() * 2, target.z - 0.5 + random.nextDouble(), 0.0, 0.5, 0.0);
                }
            }
            pressedTicks = HaemaClient.INSTANCE.getDASH_KEY().isPressed() ? pressedTicks + 1 : 0;


            long timeSinceDash = world.getTime() - lastDashed;

            float distortAmount = HaemaClient.INSTANCE.getDistortAmount();
            if (pressedTicks > 0 && canDash()) {
                HaemaClient.INSTANCE.setDistortAmount(Math.max(HaemaClient.INSTANCE.getDistortAmount() - 0.05f, -0.2f));
            } else if (timeSinceDash <= 8) {
                if (timeSinceDash == 0)
                    HaemaClient.INSTANCE.setDistortAmount(-1.4f);
                else
                    HaemaClient.INSTANCE.setDistortAmount(-0.25f + 0.25f*(float) Math.log(timeSinceDash/3f));
            } else if (distortAmount != 0) {
                if (Math.abs(distortAmount) < 0.1) {
                    HaemaClient.INSTANCE.setDistortAmount(0f);
                } else {
                    HaemaClient.INSTANCE.setDistortAmount(distortAmount - Math.copySign(0.1f, distortAmount));
                }
            }
        }
    }

    @Override
    public boolean canDash() {
        int abilityLevel = VampirableKt.getAbilityLevel(this, AbilityModule.INSTANCE.getDASH());
        return world.getTime() > lastDashed+((long) HaemaClient.INSTANCE.getDashCooldownValue() *(1+AbilityModule.INSTANCE.getDASH().getMaxLevel()-abilityLevel))
                && (
                        (VampirableKt.getVampireComponent(this)).getBlood() >= 18
                                || getAbilities().creativeMode
        )
                && abilityLevel > 0;
    }
}
