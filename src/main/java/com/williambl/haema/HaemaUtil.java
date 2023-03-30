package com.williambl.haema;

import com.google.gson.JsonElement;
import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.Dynamic;
import com.mojang.serialization.JsonOps;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;

import java.util.Optional;

public class HaemaUtil {
    public static final Codec<Tag> NBT = Codec.PASSTHROUGH
            .xmap(tag -> tag.convert(NbtOps.INSTANCE).getValue(), tag -> new Dynamic<>(NbtOps.INSTANCE, tag));
    public static final Codec<AttributeModifier> ATTRIBUTE_MODIFIER_CODEC = NBT.comapFlatMap(
            tag -> tag instanceof CompoundTag cTag ? Optional.ofNullable(AttributeModifier.load(cTag))
                    .map(DataResult::success)
                    .orElse(DataResult.error(() -> "Unable to create Attribute from tag: "+tag))
                    : DataResult.error(() -> "Not a compound tag: "+tag), AttributeModifier::save);
}
