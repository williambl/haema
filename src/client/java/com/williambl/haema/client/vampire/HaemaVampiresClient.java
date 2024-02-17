package com.williambl.haema.client.vampire;

import com.williambl.haema.client.vampire.ability.ActiveAbilitySelection;
import com.williambl.haema.client.vampire.ability.powers.dash.DashShimmerEffectManager;
import com.williambl.haema.client.vampire.ability.powers.drinking.DrinkingAbilityPowerClient;
import com.williambl.haema.client.vampire.ability.powers.hungerbar.ModifyHungerBarAbilityPowerClient;
import com.williambl.haema.client.vampire.ability.powers.vision.GlowEffectManager;
import com.williambl.haema.client.vampire.ability.powers.vision.VampireVisionFx;
import net.fabricmc.fabric.api.client.keybinding.v1.KeyBindingHelper;
import net.minecraft.client.KeyMapping;
import org.lwjgl.glfw.GLFW;

public class HaemaVampiresClient {
    public static void init() {
        Keybinds.init();
        VampireVisionFx.init();
        GlowEffectManager.INSTANCE.init();
        DashShimmerEffectManager.INSTANCE.init();
        DrinkingAbilityPowerClient.init();
        ModifyHungerBarAbilityPowerClient.init();
        ActiveAbilitySelection.init();
    }

    public static class Keybinds {
        public static final KeyMapping PRIMARY_VAMPIRE_ACTION = KeyBindingHelper.registerKeyBinding(new KeyMapping("key.haema.primary_vampire_action", GLFW.GLFW_KEY_Z, "key.category.haema"));
        public static final KeyMapping SECONDARY_VAMPIRE_ACTION = KeyBindingHelper.registerKeyBinding(new KeyMapping("key.haema.secondary_vampire_action", GLFW.GLFW_KEY_ESCAPE, "key.category.haema"));
        public static final KeyMapping SELECT_VAMPIRE_ABILITY = KeyBindingHelper.registerKeyBinding(new KeyMapping("key.haema.select_vampire_ability", GLFW.GLFW_KEY_X, "key.category.haema"));

        public static void init() {}
    }
}
