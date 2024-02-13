package com.williambl.haema;

import com.mojang.datafixers.util.Either;
import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.ItemLike;

import javax.swing.*;
import java.util.function.Consumer;
import java.util.function.Function;

public record IconProvider(Either<Icon, ItemStack> iconOrItem) {
    public static Codec<IconProvider> CODEC = Codec.either(Icon.CODEC, ItemStack.CODEC).xmap(IconProvider::new, IconProvider::iconOrItem);

    public static IconProvider of(ItemStack itemStack) {
        return new IconProvider(Either.right(itemStack.copy()));
    }

    public static IconProvider of(ItemLike item) {
        return new IconProvider(Either.right(item.asItem().getDefaultInstance()));
    }

    public static IconProvider of(ResourceLocation icon) {
        return new IconProvider(Either.left(new Icon(icon)));
    }

    public static IconProvider of(ResourceLocation icon, int size) {
        return new IconProvider(Either.left(new Icon(icon, size, size)));
    }

    public static IconProvider of(ResourceLocation icon, int xSize, int ySize) {
        return new IconProvider(Either.left(new Icon(icon, xSize, ySize)));
    }

    public void accept(Consumer<Icon> left, Consumer<ItemStack> right) {
        this.iconOrItem().<Void>map($ -> {
            left.accept($);
            return null;
        }, $ -> {
            right.accept($);
            return null;
        });
    }

    public record Icon(ResourceLocation resourceLocation, int xSize, int ySize) {
        public Icon(ResourceLocation resourceLocation) {
            this(resourceLocation, 16, 16);
        }

        private static final Codec<Icon> CODEC = Codec.either(
                        ResourceLocation.CODEC.flatComapMap(Icon::new, i -> i.xSize == i.ySize && i.xSize == 16 ? DataResult.success(i.resourceLocation()) : DataResult.<ResourceLocation>error(() -> "Cannot convert a non-16x16 icon to just ResourceLocation")),
                        RecordCodecBuilder.<Icon>create(instance -> instance.group(
                                ResourceLocation.CODEC.fieldOf("location").forGetter(Icon::resourceLocation),
                                Codec.INT.fieldOf("x_size").forGetter(Icon::xSize),
                                Codec.INT.fieldOf("y_size").forGetter(Icon::ySize)
                        ).apply(instance, Icon::new)))
                .xmap(e -> e.map(Function.identity(), Function.identity()), i -> i.xSize == i.ySize && i.xSize == 16 ? Either.left(i) : Either.right(i));
    }
}
