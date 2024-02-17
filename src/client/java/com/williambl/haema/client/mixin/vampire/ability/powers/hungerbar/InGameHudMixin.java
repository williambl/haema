package com.williambl.haema.client.mixin.vampire.ability.powers.hungerbar;

import com.llamalad7.mixinextras.injector.ModifyExpressionValue;
import com.llamalad7.mixinextras.injector.wrapoperation.WrapOperation;
import com.williambl.haema.client.vampire.ability.powers.hungerbar.ModifyHungerBarAbilityPowerClient;
import net.minecraft.client.gui.Gui;
import net.minecraft.resources.ResourceLocation;
import org.objectweb.asm.Opcodes;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;

@Mixin(Gui.class)
public abstract class InGameHudMixin {

    @ModifyExpressionValue(method = "renderPlayerHealth", at = @At(value = "FIELD", opcode = Opcodes.GETSTATIC, target = "Lnet/minecraft/client/gui/Gui;FOOD_EMPTY_HUNGER_SPRITE:Lnet/minecraft/resources/ResourceLocation;"))
    private ResourceLocation haema$modifyEmptyHungerSprite(ResourceLocation original) {
        var power = ModifyHungerBarAbilityPowerClient.activePower();
        if (power != null) {
            return power.hungerEmptySprite();
        }

        return original;
    }

    @ModifyExpressionValue(method = "renderPlayerHealth", at = @At(value = "FIELD", opcode = Opcodes.GETSTATIC, target = "Lnet/minecraft/client/gui/Gui;FOOD_HALF_HUNGER_SPRITE:Lnet/minecraft/resources/ResourceLocation;"))
    private ResourceLocation haema$modifyHalfHungerSprite(ResourceLocation original) {
        var power = ModifyHungerBarAbilityPowerClient.activePower();
        if (power != null) {
            return power.hungerHalfSprite();
        }

        return original;
    }

    @ModifyExpressionValue(method = "renderPlayerHealth", at = @At(value = "FIELD", opcode = Opcodes.GETSTATIC, target = "Lnet/minecraft/client/gui/Gui;FOOD_FULL_HUNGER_SPRITE:Lnet/minecraft/resources/ResourceLocation;"))
    private ResourceLocation haema$modifyFullHungerSprite(ResourceLocation original) {
        var power = ModifyHungerBarAbilityPowerClient.activePower();
        if (power != null) {
            return power.hungerFullSprite();
        }

        return original;
    }

    @ModifyExpressionValue(method = "renderPlayerHealth", at = @At(value = "FIELD", opcode = Opcodes.GETSTATIC, target = "Lnet/minecraft/client/gui/Gui;FOOD_EMPTY_SPRITE:Lnet/minecraft/resources/ResourceLocation;"))
    private ResourceLocation haema$modifyEmptySprite(ResourceLocation original) {
        var power = ModifyHungerBarAbilityPowerClient.activePower();
        if (power != null) {
            return power.emptySprite();
        }

        return original;
    }

    @ModifyExpressionValue(method = "renderPlayerHealth", at = @At(value = "FIELD", opcode = Opcodes.GETSTATIC, target = "Lnet/minecraft/client/gui/Gui;FOOD_HALF_SPRITE:Lnet/minecraft/resources/ResourceLocation;"))
    private ResourceLocation haema$modifyHalfSprite(ResourceLocation original) {
        var power = ModifyHungerBarAbilityPowerClient.activePower();
        if (power != null) {
            return power.halfSprite();
        }

        return original;
    }

    @ModifyExpressionValue(method = "renderPlayerHealth", at = @At(value = "FIELD", opcode = Opcodes.GETSTATIC, target = "Lnet/minecraft/client/gui/Gui;FOOD_FULL_SPRITE:Lnet/minecraft/resources/ResourceLocation;"))
    private ResourceLocation haema$modifyFullSprite(ResourceLocation original) {
        var power = ModifyHungerBarAbilityPowerClient.activePower();
        if (power != null) {
            return power.fullSprite();
        }

        return original;
    }

    @ModifyExpressionValue(method = "renderPlayerHealth", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/food/FoodData;getFoodLevel()I"))
    private int haema$modifyFoodLevel(int original) {
        if (ModifyHungerBarAbilityPowerClient.activePower() != null) {
            return (int) ModifyHungerBarAbilityPowerClient.fullnessValue();
        }

        return original;
    }

    @ModifyExpressionValue(method = "renderPlayerHealth", at = @At(value = "INVOKE", target = "Lnet/minecraft/world/food/FoodData;getSaturationLevel()F"))
    private float haema$myIconsDontWiggleWiggle(float original) {
        if (ModifyHungerBarAbilityPowerClient.activePower() != null) {
            return 1;
        }

        return original;
    }
}
