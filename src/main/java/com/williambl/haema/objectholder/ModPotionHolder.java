package com.williambl.haema.objectholder;

import com.williambl.haema.Haema;
import net.minecraft.potion.Potion;
import net.minecraftforge.registries.ObjectHolder;

@ObjectHolder(Haema.MODID)
public class ModPotionHolder {

    @ObjectHolder("vampiric_strength")
    public static Potion vampiric_strength;

    @ObjectHolder("vampiric_weakness")
    public static Potion vampiric_weakness;

    @ObjectHolder("minecraft:invisibility")
    public static Potion invisibility;
}
