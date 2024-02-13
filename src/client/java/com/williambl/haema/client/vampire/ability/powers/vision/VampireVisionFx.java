package com.williambl.haema.client.vampire.ability.powers.vision;

import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.vampire.ability.powers.vision.VampireVisionVampireAbilityPower;
import ladysnake.satin.api.event.ShaderEffectRenderCallback;
import ladysnake.satin.api.managed.ManagedShaderEffect;
import ladysnake.satin.api.managed.ShaderEffectManager;
import net.fabricmc.fabric.api.client.event.lifecycle.v1.ClientTickEvents;
import net.minecraft.client.Minecraft;

import static com.williambl.haema.Haema.id;

public class VampireVisionFx implements ShaderEffectRenderCallback, ClientTickEvents.StartTick {
    private final ManagedShaderEffect shader = ShaderEffectManager.getInstance().manage(id("shaders/post/vampire_vision.json"));

    public void init() {
        ShaderEffectRenderCallback.EVENT.register(this);
        ClientTickEvents.START_CLIENT_TICK.register(this);
    }

    public void setRed(float amount) {
        this.shader.setUniformValue("RedMatrix", amount, 0f, 0f);
    }

    public void setBrightnessAdjust(float amount) {
        this.shader.setUniformValue("BrightnessAdjust", amount); //TODO config
    }

    public void setSaturation(float amount) {
        this.shader.setUniformValue("Saturation", amount); //TODO config
    }
    //TODO config

    @Override
    public void renderShaderEffects(float tickDelta) {
        var cam = Minecraft.getInstance().cameraEntity;
        if (cam == null) {
            return;
        }

        var comp = VampireAbilitiesComponent.KEY.getNullable(cam);
        if (comp != null && !comp.getEnabledPowersOfClass(VampireVisionVampireAbilityPower.class).isEmpty()) {
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
        if (vampire == null || abilities == null || abilities.getEnabledPowersOfClass(VampireVisionVampireAbilityPower.class).isEmpty()) {
            return;
        }

        float bloodLevel = (float) (vampire.getBlood() / VampireComponent.MAX_BLOOD);

        this.setSaturation(0.8f * bloodLevel);
        this.setBrightnessAdjust(bloodLevel / 4f + 0.05f);
        //TODO set red based on last fed
    }
}
