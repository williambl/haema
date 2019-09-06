package com.williambl.haema.objectholder;

import com.williambl.haema.Haema;
import net.minecraft.potion.Potion;
import net.minecraftforge.fml.common.registry.GameRegistry;

@GameRegistry.ObjectHolder(Haema.MODID)
public class ModPotionHolder {

    @GameRegistry.ObjectHolder("vampiric_strength")
    public static Potion vampiric_strength;

    @GameRegistry.ObjectHolder("vampiric_weakness")
    public static Potion vampiric_weakness;

    @GameRegistry.ObjectHolder("minecraft:invisibility")
    public static Potion invisibility;
}
