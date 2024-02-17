package com.williambl.haema.client.vampire.ability.powers.dash;

import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.dash.DashAbilityPower;
import com.williambl.haema.vampire.ability.powers.vision.VampireVisionVampireAbilityPower;
import ladysnake.satin.api.event.ShaderEffectRenderCallback;
import ladysnake.satin.api.managed.ManagedShaderEffect;
import ladysnake.satin.api.managed.ShaderEffectManager;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.minecraft.client.Minecraft;
import net.minecraft.util.Mth;

import static com.williambl.haema.Haema.id;

public class DashWobbleFx implements ShaderEffectRenderCallback, ClientTickEvents.StartTick {
    public static final DashWobbleFx INSTANCE = new DashWobbleFx();
    private boolean isRenderingThisTick = false;
    private float prevDistortAmount = 0;
    private float distortAmount = 0;

    public static void init() {
        ShaderEffectRenderCallback.EVENT.register(INSTANCE);
        ClientTickEvents.START_CLIENT_TICK.register(INSTANCE);
    }

    private final ManagedShaderEffect shader = ShaderEffectManager.getInstance().manage(id("shaders/post/dash_wobble.json"));

    public void setDistortAmount(float amount) {
        this.prevDistortAmount = this.distortAmount;
        this.distortAmount = amount;
    }

    private float getDistortAmount() {
        return this.distortAmount;
    }

    @Override
    public void renderShaderEffects(float tickDelta) {
        var cam = Minecraft.getInstance().cameraEntity;
        if (cam == null) {
            return;
        }

        if (this.isRenderingThisTick) {
            this.shader.setUniformValue("DistortAmount", Mth.lerp(tickDelta, this.prevDistortAmount, this.distortAmount));
            this.shader.render(tickDelta);
        }
    }

    @Override
    public void onStartTick(Minecraft client) {
        var cam = client.cameraEntity;
        if (cam == null) {
            return;
        }

        var vampire = VampireComponent.KEY.getNullable(cam);
        var abilities = VampireAbilitiesComponent.KEY.getNullable(cam);
        if (vampire == null || abilities == null || abilities.getEnabledPowersOfClass(DashAbilityPower.class).isEmpty()) {
            this.isRenderingThisTick = false;
            return;
        }

        this.isRenderingThisTick = true;

        if (DashAbilityPowerClient.pressedTicks > 0) {
            this.setDistortAmount(Math.max(this.getDistortAmount() - 0.05f, -0.2f) + cam.level().getRandom().nextFloat()*0.03f);
        } else if (DashAbilityPowerClient.ticksSinceDashed <= 8) {
            this.setDistortAmount(DashAbilityPowerClient.ticksSinceDashed == 0 ? -1.4f : -0.25f + 0.25f * (float) Math.log(((DashAbilityPowerClient.ticksSinceDashed) / 3f)));
        } else if (this.distortAmount != 0) {
            if (Math.abs(this.distortAmount) < 0.1) {
                this.setDistortAmount(0.0f);
            } else {
                this.setDistortAmount(this.getDistortAmount() - Math.copySign(0.1f, this.getDistortAmount()));
            }
        }
    }
}
