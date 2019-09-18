package com.williambl.haema.objectholder;

import com.williambl.haema.Haema;
import net.minecraft.potion.Effect;
import net.minecraftforge.registries.ObjectHolder;

@ObjectHolder(Haema.MODID)
public class ModEffectHolder {

    @ObjectHolder("vampiric_strength")
    public static Effect vampiric_strength;

    @ObjectHolder("vampiric_weakness")
    public static Effect vampiric_weakness;

    @ObjectHolder("minecraft:invisibility")
    public static Effect invisibility;

    @ObjectHolder("minecraft:weakness")
    public static Effect weakness;
}
