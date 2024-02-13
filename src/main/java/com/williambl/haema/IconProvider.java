package com.williambl.haema;

import com.mojang.datafixers.util.Either;
import com.mojang.serialization.Codec;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.ItemLike;

import javax.swing.*;
import java.util.function.Consumer;

public record IconProvider(Either<ResourceLocation, ItemStack> iconOrItem) {
    public static Codec<IconProvider> CODEC = Codec.either(ResourceLocation.CODEC, ItemStack.CODEC).xmap(IconProvider::new, IconProvider::iconOrItem);

    public static IconProvider of(ItemStack itemStack) {
        return new IconProvider(Either.right(itemStack.copy()));
    }

    public static IconProvider of(ItemLike item) {
        return new IconProvider(Either.right(item.asItem().getDefaultInstance()));
    }

    public static IconProvider of(ResourceLocation icon) {
        return new IconProvider(Either.left(icon));
    }

    public void accept(Consumer<ResourceLocation> left, Consumer<ItemStack> right) {
        this.iconOrItem().<Void>map($ -> {
            left.accept($);
            return null;
        }, $ -> {
            right.accept($);
            return null;
        });
    }
}
