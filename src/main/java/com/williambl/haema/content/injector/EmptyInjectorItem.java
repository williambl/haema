package com.williambl.haema.content.injector;

import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.content.HaemaContent;
import net.fabricmc.fabric.api.transfer.v1.context.ContainerItemContext;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidStorage;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
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
        var res = this.tryInject(player, ContainerItemContext.forPlayerInteraction(player, interactionHand));
        return res ? InteractionResultHolder.success(player.getItemInHand(interactionHand))
                : InteractionResultHolder.pass(player.getItemInHand(interactionHand));
    }

    private boolean tryInject(LivingEntity target, ContainerItemContext injectorStorageLookup) {
        if (target.level().isClientSide()) {
            return false;
        }


        var injectorStorage = injectorStorageLookup.find(FluidStorage.ITEM);
        var entityStorage = BloodApi.getBloodStorage(target);
        if (entityStorage.isEmpty()) {
            return false;
        }

        if (HaemaUtil.moveFluid(entityStorage.get(), injectorStorage)) {
            target.playSound(SoundEvents.BOTTLE_FILL);
            var component = VampireComponent.KEY.getNullable(target);
            var source = target.level().registryAccess().registry(VampirismSource.REGISTRY_KEY)
                    .flatMap(r -> r.getOptional(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR));
            if (component != null && component.isVampire() && source.isPresent()) {
                component.tryCure(source.get());
            }

            return true;
        }

        return false;
    }
}