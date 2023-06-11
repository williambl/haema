package com.williambl.haema.content;

import net.minecraft.Util;
import net.minecraft.network.chat.Component;
import net.minecraft.util.StringRepresentable;

import static com.williambl.haema.Haema.id;

public enum BloodQuality implements StringRepresentable {
    DISGUSTING(0xa10000, "disgusting"),
    BAD(0xa25203, "bad"),
    MIDDLING(0xa1a100, "middling"),
    PALATABLE(0x416600, "palatable"),
    GOOD(0x078446, "good"),
    EXCELLENT(0x440a7f, "excellent"),
    PERFECT(0xa41f62, "perfect"),
    ;

    public final int colour;
    private final String name;
    public final String translationKey;

    BloodQuality(int colour, String name) {
        this.colour = colour;
        this.name = name;
        this.translationKey = Util.makeDescriptionId("blood_quality", id(name));
    }

    public Component text() {
        return Component.translatable(translationKey).withStyle(s -> s.withColor(colour));
    }

    @Override
    public String getSerializedName() {
        return this.name;
    }
}
