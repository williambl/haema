package com.williambl.haema.client.mixin.vampire.ability.powers;

import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.client.vampire.ability.powers.VampireAbilityPowerTickKeybindsCallback;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import org.jetbrains.annotations.Nullable;
import org.objectweb.asm.Opcodes;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.Slice;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Minecraft.class)
public class MinecraftMixin {
    @Shadow @Nullable public LocalPlayer player;

    @Inject(method = "handleKeybinds",
            slice = @Slice(
                    from = @At(value = "FIELD", opcode = Opcodes.GETFIELD, target = "Lnet/minecraft/client/Options;keyPickItem:Lnet/minecraft/client/KeyMapping;", ordinal = 0),
                    to = @At(value = "FIELD", opcode = Opcodes.GETFIELD, target = "Lnet/minecraft/client/Options;keyPickItem:Lnet/minecraft/client/KeyMapping;", ordinal = 1)
            ),
            at = @At(value = "FIELD", opcode = Opcodes.GETFIELD, target = "Lnet/minecraft/client/Options;keyAttack:Lnet/minecraft/client/KeyMapping;"))
    private void haema$triggerPowerKeybinds(CallbackInfo ci) {
        assert this.player != null; // we know it's not null here
        var component = this.player.getComponent(VampireAbilitiesComponent.KEY);
        var abilities = component.getEnabledAbilities();
        VampireAbilityPowerTickKeybindsCallback.invokeAll(abilities, this.player, component.getActiveAbility().orElse(null));
    }
}
