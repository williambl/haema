package com.williambl.haema.client.vampire.ability;

import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.client.vampire.HaemaVampiresClient;
import com.williambl.haema.vampire.ability.SetActiveAbilityPacket;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.minecraft.Util;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.Holder.Reference;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FastColor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Matrix4f;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class AbilitySelectionScreen extends Screen {
    private static final int OFFSET = 50;
    private static final int HIGHLIGHT_COLOUR = -2130706433;
    private List<AbilityOption> abilityOptions = List.of();
    private @Nullable AbilityOption selectedOption = null;

    protected AbilitySelectionScreen(Component component) {
        super(component);
    }

    @Override
    protected void init() {
        super.init();
        if (this.minecraft != null && this.minecraft.player != null) {
            var abilities = this.minecraft.player.getComponent(VampireAbilitiesComponent.KEY).getEnabledAbilities().stream()
                    .filter(a -> a.value().canBeActive())
                    .toList();
            double[] angles = getAngles(abilities.size());
            double halfAngle = getAngle(abilities.size(), 1)/2;
            this.abilityOptions = new ArrayList<>();
            for (var ability : abilities) {
                int i = this.abilityOptions.size();
                int[] coords = polarToCartesian(angles[i] + halfAngle, OFFSET);
                this.abilityOptions.add(new AbilityOption(ability,
                        Component.translatable(Util.makeDescriptionId("vampire_ability", ability.key().location())),
                        coords[0],
                        coords[1]));
            }
        }
    }

    @Override
    public boolean keyReleased(int i, int j, int k) {
        if (HaemaVampiresClient.Keybinds.SELECT_VAMPIRE_ABILITY.matches(i, j) && this.minecraft != null) {
            this.minecraft.setScreen(null);
            this.minecraft.mouseHandler.grabMouse();
            ClientPlayNetworking.send(new SetActiveAbilityPacket(Optional.ofNullable(this.selectedOption).map(o -> o.ability().key())));
            return true;
        }
        return super.keyReleased(i, j, k);
    }

    @Override
    public void render(@NotNull GuiGraphics guiGraphics, int mouseX, int mouseY, float tickDelta) {
        super.render(guiGraphics, mouseX, mouseY, tickDelta);
        int centerX = this.width/2;
        int centerY = this.height/2;
        boolean onlyOneOption = this.abilityOptions.size() == 1;

        for (AbilityOption ability : this.abilityOptions) {
            int x = onlyOneOption ? centerX : centerX - ability.x;
            int y = onlyOneOption ? centerY : ability.y + centerY;
            ability.ability().value().icon().accept(
                    icon -> guiGraphics.blit(icon.resourceLocation(), x-icon.xSize()/2, y-icon.ySize()/2, 0, 0, icon.xSize(), icon.ySize(), icon.xSize(), icon.ySize()),
                    item -> guiGraphics.renderFakeItem(item, x-8, y-8));
        }


        if (!this.abilityOptions.isEmpty()) {
            double rawMouseTheta = Math.atan2(mouseY - centerY, centerX - mouseX);
            double positiveMouseTheta = rawMouseTheta < 0 ? rawMouseTheta + Math.PI*2 : rawMouseTheta;
            double sector = (this.abilityOptions.size() * positiveMouseTheta / (Math.PI * 2.0));
            this.selectedOption = this.abilityOptions.get((int) sector);
            this.setTooltipForNextRenderPass(this.selectedOption.name);

            guiGraphics.drawManaged(() -> {
                var vertexConsumer = guiGraphics.bufferSource().getBuffer(RenderType.gui());
                var pose = guiGraphics.pose();
                Matrix4f matrix4f = pose.last().pose();

                float a = (float) FastColor.ARGB32.alpha(HIGHLIGHT_COLOUR) / 255.0F;
                float r = (float) FastColor.ARGB32.red(HIGHLIGHT_COLOUR) / 255.0F;
                float g = (float) FastColor.ARGB32.green(HIGHLIGHT_COLOUR) / 255.0F;
                float b = (float) FastColor.ARGB32.blue(HIGHLIGHT_COLOUR) / 255.0F;

                float m = 100;
                if (onlyOneOption) {
                    vertexConsumer.vertex(matrix4f, 0, 0, m).color(r, g, b, a).endVertex();
                    vertexConsumer.vertex(matrix4f, 0, this.height, m).color(r, g, b, a).endVertex();
                    vertexConsumer.vertex(matrix4f, this.width, this.height, m).color(r, g, b, a).endVertex();
                    vertexConsumer.vertex(matrix4f, this.width, 0, m).color(r, g, b, a).endVertex();
                } else {
                    double minTheta = getAngle(this.abilityOptions.size(), (int) Math.floor(sector));
                    double maxTheta = getAngle(this.abilityOptions.size(), (int) Math.floor(sector) + 1);
                    double avgTheta = (minTheta+maxTheta)/2.0;
                    double length = Math.max(this.width, this.height);
                    int[] centre = new int[]{centerX, centerY};
                    int[] leftCorner = polarToCartesian(minTheta, length);
                    int[] rightCorner = polarToCartesian(maxTheta, length);
                    int[] farCorner = polarToCartesian(avgTheta, length);
                    vertexConsumer.vertex(matrix4f, centre[0], centre[1], m).color(r, g, b, a).endVertex();
                    vertexConsumer.vertex(matrix4f, centre[0] - leftCorner[0], centre[1] + leftCorner[1], m).color(r, g, b, a).endVertex();
                    vertexConsumer.vertex(matrix4f, centre[0] - farCorner[0], centre[1] + farCorner[1], m).color(r, g, b, a).endVertex();
                    vertexConsumer.vertex(matrix4f, centre[0] - rightCorner[0], centre[1] + rightCorner[1], m).color(r, g, b, a).endVertex();
                }
            });
        }
    }

    private static int[] polarToCartesian(double theta, double r) {
        return new int[]{(int) (Math.cos(theta) * r), (int) (Math.sin(theta) * r)};
    }

    private static double[] getAngles(int n) {
        double[] values = new double[n];
        for (int k = 0; k < n; k++) {
            values[k] = getAngle(n, k);
        }

        return values;
    }

    private static double getAngle(int n, int k) {
        return k * 2.0 * Math.PI / n;
    }

    private record AbilityOption(Reference<VampireAbility> ability, Component name, int x, int y) {

    }
}
