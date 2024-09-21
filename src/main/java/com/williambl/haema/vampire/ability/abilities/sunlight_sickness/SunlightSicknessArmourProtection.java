package com.williambl.haema.vampire.ability.abilities.sunlight_sickness;

import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.vampire.ability.powers.sunlight_sickness.VampireBurnEvents;
import com.williambl.haema.vampire.HaemaVampires;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.level.GameRules;

public class SunlightSicknessArmourProtection {
    public static void init() {
        VampireBurnEvents.PREVENT.register(VampireBurnEvents.PREVENT_PHASE_ARMOUR, vampire -> {
            GameRules gameRules = vampire.level().getGameRules();

            if (!gameRules.getBoolean(HaemaVampires.VampireGameRules.ARMOUR_PROTECTS)) {
                return false;
            }

            if (!(gameRules.getBoolean(HaemaVampires.VampireGameRules.NEED_FULL_ARMOUR_TO_PROTECT))
                    || HaemaUtil.stream(vampire.getArmorSlots()).allMatch(i -> i.is(HaemaVampires.VampireTags.VAMPIRE_PROTECTIVE_CLOTHING))) {
                HaemaUtil.forEachIndexed(vampire.getArmorSlots(), (i, stack) -> {
                    if (vampire.getRandom().nextFloat() < 0.025
                            && gameRules.getBoolean(HaemaVampires.VampireGameRules.ARMOUR_DAMAGED_BY_BURNING)) {
                        stack.hurtAndBreak((int) (vampire.getRandom().nextFloat() * (i + 2)), vampire, v ->
                                v.playSound(SoundEvents.GENERIC_BURN, 1f, 1f));
                    }
                });

                return true;
            }

            return false;
        });
    }
}
