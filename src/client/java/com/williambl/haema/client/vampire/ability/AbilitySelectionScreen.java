package com.williambl.haema.client.vampire.ability;

import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.client.vampire.HaemaVampiresClient;
import net.minecraft.Util;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FastColor;
import org.jetbrains.annotations.NotNull;
import org.joml.Matrix4f;

import java.util.ArrayList;
import java.util.List;

public class AbilitySelectionScreen extends Screen {
    private static final int OFFSET = 50;
    private static final int HIGHLIGHT_COLOUR = -2130706433;
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
    public void render(@NotNull GuiGraphics guiGraphics, int mouseX, int mouseY, float tickDelta) {
        super.render(guiGraphics, mouseX, mouseY, tickDelta);
        int centerX = this.width/2;
        int centerY = this.height/2;
        for (AbilityOption ability : this.abilityOptions) {
            int x = ability.x + centerX;
            int y = ability.y + centerY;
            ability.ability().icon().accept(
                    icon -> guiGraphics.blit(icon.resourceLocation(), x-icon.xSize(), y-icon.ySize(), 0, 0, icon.xSize(), icon.ySize(), icon.xSize(), icon.ySize()),
                    item -> guiGraphics.renderFakeItem(item, x-16, y-16));
        }


        if (!this.abilityOptions.isEmpty()) {
            double mouseTheta = Math.atan2(mouseY - centerY, mouseX - centerX) + Math.PI;
            double sector = (mouseTheta / (Math.PI * 2.0) * this.abilityOptions.size());
            var closestAbility = this.abilityOptions.get(Math.floorMod(((int) sector) - 1, this.abilityOptions.size()));
            this.setTooltipForNextRenderPass(closestAbility.name);

            guiGraphics.drawManaged(() -> {
                var vertexConsumer = guiGraphics.bufferSource().getBuffer(RenderType.gui());
                var pose = guiGraphics.pose();
                Matrix4f matrix4f = pose.last().pose();

                float a = (float) FastColor.ARGB32.alpha(HIGHLIGHT_COLOUR) / 255.0F;
                float r = (float) FastColor.ARGB32.red(HIGHLIGHT_COLOUR) / 255.0F;
                float g = (float) FastColor.ARGB32.green(HIGHLIGHT_COLOUR) / 255.0F;
                float b = (float) FastColor.ARGB32.blue(HIGHLIGHT_COLOUR) / 255.0F;

                float m = 100;
                double minTheta = ((Math.floor(sector) / this.abilityOptions.size()) * Math.PI * 2.0) - Math.PI;
                double maxTheta = ((Math.ceil(sector) / this.abilityOptions.size()) * Math.PI * 2.0) - Math.PI;
                double length = Math.max(this.width, this.height);
                int[] centre = new int[]{centerX, centerY};
                int[] leftCorner = polarToCartesian(minTheta, length);
                int[] rightCorner = polarToCartesian(maxTheta, length);
                int[] farCorner = polarToCartesian(mouseTheta - Math.PI, length);
                vertexConsumer.vertex(matrix4f, centre[0], centre[1], m).color(r, g, b, a).endVertex();
                vertexConsumer.vertex(matrix4f, centre[0] + rightCorner[0], centre[1] + rightCorner[1], m).color(r, g, b, a).endVertex();
                vertexConsumer.vertex(matrix4f, centre[0] + farCorner[0], centre[1] + farCorner[1], m).color(r, g, b, a).endVertex();
                vertexConsumer.vertex(matrix4f, centre[0] + leftCorner[0], centre[1] + leftCorner[1], m).color(r, g, b, a).endVertex();
            });
        }
    }

    private static int[] polarToCartesian(double theta, double r) {
        return new int[]{(int) (Math.cos(theta) * r), (int) (Math.sin(theta) * r)};
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
