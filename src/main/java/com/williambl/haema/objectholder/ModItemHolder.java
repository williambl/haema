package com.williambl.haema.objectholder;

import com.williambl.haema.Haema;
import net.minecraft.item.Item;
import net.minecraftforge.fml.common.registry.GameRegistry;

@GameRegistry.ObjectHolder(Haema.MODID)
public class ModItemHolder {

    @GameRegistry.ObjectHolder("thermalfoundation:tool.sword_silver")
    public static Item silver_sword;

    @GameRegistry.ObjectHolder("thermalfoundation:tool.shovel_silver")
    public static Item silver_shovel;

    @GameRegistry.ObjectHolder("thermalfoundation:tool.axe_silver")
    public static Item silver_axe;

    @GameRegistry.ObjectHolder("thermalfoundation:tool.pickaxe_silver")
    public static Item silver_pickaxe;

    @GameRegistry.ObjectHolder("thermalfoundation:tool.hoe_silver")
    public static Item silver_hoe;

    @GameRegistry.ObjectHolder("thermalfoundation:tool.hammer_silver")
    public static Item silver_hammer;

    @GameRegistry.ObjectHolder("thermalfoundation:tool.sickle_silver")
    public static Item silver_sickle;

    @GameRegistry.ObjectHolder("thermalfoundation:tool.excavator_silver")
    public static Item silver_excavator;
}
