package com.williambl.haema.client.vampire.ability;

import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.client.vampire.HaemaVampiresClient;
import net.minecraft.Util;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.network.chat.Component;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class AbilitySelectionScreen extends Screen {
    private static final int OFFSET = 50;
    private List<AbilityOption> abilityOptions = List.of();
    protected AbilitySelectionScreen(Component component) {
        super(component);
    }

    @Override
    protected void init() {
        super.init();
        if (this.minecraft != null && this.minecraft.player != null) {
            var abilities = this.minecraft.player.getComponent(VampireAbilitiesComponent.KEY).getAbilities();
            var rootsOfUnity = rootsOfUnity(abilities.size());
            var registries = this.minecraft.player.level().registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY);
            this.abilityOptions = new ArrayList<>();
            for (var ability : abilities) {
                int i = this.abilityOptions.size();
                this.abilityOptions.add(new AbilityOption(ability,
                        Component.translatable(Util.makeDescriptionId("vampire_ability", registries.getKey(ability))),
                        (int) (rootsOfUnity[i][0] * 50),
                        (int) (rootsOfUnity[i][1] * 50)));
            }
        }
    }

    @Override
    public boolean keyReleased(int i, int j, int k) {
        if (HaemaVampiresClient.Keybinds.SELECT_VAMPIRE_ABILITY.matches(i, j) && this.minecraft != null) {
            this.minecraft.setScreen(null);
            this.minecraft.mouseHandler.grabMouse();
            return true;
        }
        return super.keyReleased(i, j, k);
    }

    @Override
    public void render(@NotNull GuiGraphics guiGraphics, int i, int j, float f) {
        super.render(guiGraphics, i, j, f);
        int centerX = this.width/2;
        int centerY = this.height/2;
        for (AbilityOption ability : this.abilityOptions) {
            int x = ability.x + centerX;
            int y = ability.y + centerY;
            guiGraphics.drawCenteredString(this.font, ability.name, x, y, 0xffffff);
        }

        if (!this.abilityOptions.isEmpty()) {
            double mouseTheta = Math.atan2(j - centerY, i - centerX) + Math.PI;
            double sector = (mouseTheta / (Math.PI * 2.0) * this.abilityOptions.size());
            var closestAbility = this.abilityOptions.get(Math.floorMod(((int) sector) - 1, this.abilityOptions.size()));
            this.setTooltipForNextRenderPass(closestAbility.name);
        }
    }

    private static double[][] rootsOfUnity(int n) {
        double[][] values = new double[n][];
        for (int k = 0; k < n; k++) {
            values[k] = new double[]{Math.cos((2.0 * k * Math.PI) / (double) n), Math.sin((2.0 * k * Math.PI) / (double) n)};
        }

        return values;
    }

    private record AbilityOption(VampireAbility ability, Component name, int x, int y) {

    }
}
