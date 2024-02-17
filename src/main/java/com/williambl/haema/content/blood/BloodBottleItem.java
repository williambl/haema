package com.williambl.haema.content.blood;

import com.williambl.haema.api.content.blood.BloodQuality;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class BloodBottleItem extends Item {
    private final BloodQuality quality;
    public static final String DESCRIPTION_TRANSLATION_KEY = "item.haema.blood_bottle.desc";

    public BloodBottleItem(Properties properties, BloodQuality quality) {
        super(properties);
        this.quality = quality;
    }

    @Override
    public void appendHoverText(ItemStack itemStack, @Nullable Level level, List<Component> tooltip, TooltipFlag tooltipFlag) {
        super.appendHoverText(itemStack, level, tooltip, tooltipFlag);
        tooltip.add(Component.translatable(DESCRIPTION_TRANSLATION_KEY, this.quality.text()).withStyle(ChatFormatting.GRAY));
    }
}