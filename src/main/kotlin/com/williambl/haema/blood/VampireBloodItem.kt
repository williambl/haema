package com.williambl.haema.blood

import com.williambl.haema.Haema
import net.minecraft.client.item.TooltipContext
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.world.World

class VampireBloodItem : Item(Settings().group(Haema.ITEM_GROUP)) {
    override fun appendTooltip(
        stack: ItemStack?,
        world: World?,
        tooltip: MutableList<Text>,
        context: TooltipContext?
    ) {
        super.appendTooltip(stack, world, tooltip, context)
        tooltip.add(TranslatableText("item.haema.vampire_blood.desc").formatted(Formatting.DARK_RED))
    }
}