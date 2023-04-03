package com.williambl.haema;

import com.google.gson.JsonElement;
import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.Dynamic;
import com.mojang.serialization.JsonOps;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;

import java.util.Optional;

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
}
