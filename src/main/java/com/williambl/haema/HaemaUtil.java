package com.williambl.haema;

import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;

import java.util.Optional;
import java.util.function.Predicate;

public class HaemaUtil {
    public static final Codec<AttributeModifier> ATTRIBUTE_MODIFIER_CODEC = CompoundTag.CODEC.comapFlatMap(
            tag -> Optional.ofNullable(AttributeModifier.load(tag))
                    .map(DataResult::success)
                    .orElse(DataResult.error(() -> "Unable to create Attribute Modifier from tag: "+tag)),
            AttributeModifier::save);
    public static final Codec<MobEffectInstance> MOB_EFFECT_INSTANCE_CODEC = CompoundTag.CODEC.comapFlatMap(
            tag -> Optional.ofNullable(MobEffectInstance.load(tag))
                    .map(DataResult::success)
                    .orElse(DataResult.error(() -> "Unable to create MobEffectInstance from tag: "+tag)),
                    instance -> instance.save(new CompoundTag()));

    public static <T> Predicate<ResourceKey<T>> checkInRegistry(Registry<T> registry, String logMessage) {
        return (t) -> {
            if (!registry.containsKey(t)) {
                Haema.LOGGER.warn(logMessage, t.location());
                return false;
            }
            return true;
        };
    }
}
