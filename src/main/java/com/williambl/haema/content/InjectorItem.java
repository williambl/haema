package com.williambl.haema.content;

import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class InjectorItem extends Item {
    private final BloodQuality quality;
    public static final String DESCRIPTION_TRANSLATION_KEY = "item.haema.injector.desc";

    public InjectorItem(Properties properties, BloodQuality quality) {
        super(properties);
        this.quality = quality;
    }

    @Override
    public void appendHoverText(ItemStack itemStack, @Nullable Level level, List<Component> tooltip, TooltipFlag tooltipFlag) {
        super.appendHoverText(itemStack, level, tooltip, tooltipFlag);
        tooltip.add(Component.translatable(DESCRIPTION_TRANSLATION_KEY, this.quality.text()).withStyle(ChatFormatting.GRAY));
    }
}