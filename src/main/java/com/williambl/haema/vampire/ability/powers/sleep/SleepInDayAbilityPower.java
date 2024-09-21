package com.williambl.haema.vampire.ability.powers.sleep;

import com.mojang.serialization.Codec;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.api.vampire.ability.VampireAbilityPower;
import net.fabricmc.fabric.api.entity.event.v1.EntitySleepEvents;
import net.minecraft.util.KeyDispatchDataCodec;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.LivingEntity;

public record SleepInDayAbilityPower() implements VampireAbilityPower {
    public static final KeyDispatchDataCodec<SleepInDayAbilityPower> CODEC = KeyDispatchDataCodec.of(Codec.unit(SleepInDayAbilityPower::new));

    @Override
    public void apply(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public void tick(LivingEntity entity, VampireAbility source, boolean isActive) {
    }

    @Override
    public void remove(LivingEntity entity, VampireAbility source) {
    }

    @Override
    public KeyDispatchDataCodec<SleepInDayAbilityPower> codec() {
        return CODEC;
    }

    public static void init() {
        EntitySleepEvents.ALLOW_SLEEP_TIME.register((player, sleepingPos, vanillaResult) -> {
            //TODO somehow change the message
            if (!VampireAbilitiesComponent.KEY.get(player).getEnabledPowersOfClass(SleepInDayAbilityPower.class).isEmpty()) {
                return player.level().isDay() ? InteractionResult.SUCCESS : InteractionResult.FAIL;
            } else {
                return InteractionResult.PASS;
            }
        });
    }
}
