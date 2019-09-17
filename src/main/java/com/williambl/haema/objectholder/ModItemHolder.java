package com.williambl.haema.objectholder;

import com.williambl.haema.Haema;
import net.minecraft.item.Item;
import net.minecraftforge.registries.ObjectHolder;

@ObjectHolder(Haema.MODID)
public class ModItemHolder {

    @ObjectHolder("thermalfoundation:tool.sword_silver")
    public static Item silver_sword;

    @ObjectHolder("thermalfoundation:tool.shovel_silver")
    public static Item silver_shovel;

    @ObjectHolder("thermalfoundation:tool.axe_silver")
    public static Item silver_axe;

    @ObjectHolder("thermalfoundation:tool.pickaxe_silver")
    public static Item silver_pickaxe;

    @ObjectHolder("thermalfoundation:tool.hoe_silver")
    public static Item silver_hoe;

    @ObjectHolder("thermalfoundation:tool.hammer_silver")
    public static Item silver_hammer;

    @ObjectHolder("thermalfoundation:tool.sickle_silver")
    public static Item silver_sickle;

    @ObjectHolder("thermalfoundation:tool.excavator_silver")
    public static Item silver_excavator;
}
