package com.williambl.haema.content.injector;

import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.content.HaemaContent;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;

public class EmptyInjectorItem extends Item {
    public static final String DESCRIPTION_TRANSLATION_KEY = "item.haema.empty_injector.desc";
    public EmptyInjectorItem(Properties properties) {
        super(properties);
    }

    @Override
    public void appendHoverText(ItemStack itemStack, @Nullable Level level, List<Component> tooltip, TooltipFlag tooltipFlag) {
        super.appendHoverText(itemStack, level, tooltip, tooltipFlag);
        tooltip.add(Component.translatable(DESCRIPTION_TRANSLATION_KEY).withStyle(ChatFormatting.GRAY));
    }

    @Override
    public InteractionResultHolder<ItemStack> use(Level level, Player player, InteractionHand interactionHand) {
        var res = tryInject(player);
        return res.map(InteractionResultHolder::success).orElseGet(() ->
                InteractionResultHolder.pass(player.getItemInHand(interactionHand)));
    }

    private Optional<ItemStack> tryInject(LivingEntity entity) {
        if (entity.level().isClientSide()) {
            return Optional.empty();
        }


        Fluid extracted = BloodApi.extractBlood(entity, HaemaContent.ContentConstants.INJECTOR_CAPACITY_DROPLETS);
        var quality = BloodApi.getBloodQuality(extracted);
        if (quality.isEmpty()) {
            return Optional.empty();
        }

        var component = VampireComponent.KEY.getNullable(entity);
        var source = entity.level().registryAccess().registry(VampirismSource.REGISTRY_KEY)
                .flatMap(r -> r.getOptional(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR));
        if (component != null && component.isVampire() && source.isPresent()) {
            component.tryCure(source.get());
        }

        return Optional.of(new ItemStack(HaemaContent.ContentItems.INJECTORS.get(quality.get())));
    }
}