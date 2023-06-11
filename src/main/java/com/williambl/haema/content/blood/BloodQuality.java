package com.williambl.haema.content.blood;

import net.minecraft.Util;
import net.minecraft.network.chat.Component;
import net.minecraft.util.StringRepresentable;

import static com.williambl.haema.Haema.id;

public enum BloodQuality implements StringRepresentable {
    DISGUSTING(0xa10000, "disgusting", false, 0.1),
    BAD(0xa25203, "bad", false, 0.2),
    MIDDLING(0xa1a100, "middling", false, 0.5),
    PALATABLE(0x416600, "palatable", false, 0.7),
    GOOD(0x078446, "good", false, 0.8),
    EXCELLENT(0x440a7f, "excellent", true, 1.0),
    PERFECT(0xa41f62, "perfect", true, 2.0),
    ;

    public final int colour;
    private final String name;
    public final String translationKey;
    public final boolean vampiric;
    public final double multiplier;

    BloodQuality(int colour, String name, boolean vampiric, double multiplier) {
        this.colour = colour;
        this.name = name;
        this.translationKey = Util.makeDescriptionId("blood_quality", id(name));
        this.vampiric = vampiric;
        this.multiplier = multiplier;
    }

    public Component text() {
        return Component.translatable(translationKey).withStyle(s -> s.withColor(colour));
    }

    @Override
    public String getSerializedName() {
        return this.name;
    }
}
