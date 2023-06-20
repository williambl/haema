package com.williambl.haema.content.injector;

import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.content.HaemaContent;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
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
    public InteractionResultHolder<ItemStack> use(Level level, Player player, InteractionHand interactionHand) {
        var stack = player.getItemInHand(interactionHand);
        var result = tryInject(player);
        return switch (result) {
            case NOTHING -> InteractionResultHolder.pass(stack);
            case INJECTED, CONVERTED ->
                    InteractionResultHolder.success(HaemaContent.ContentItems.EMPTY_INJECTOR.getDefaultInstance());
            case INCOMPATIBLE -> {
                giveIncompatibleBloodEffects(player);
                yield  InteractionResultHolder.success(HaemaContent.ContentItems.EMPTY_INJECTOR.getDefaultInstance());
            }
        };
    }

    private void giveIncompatibleBloodEffects(LivingEntity entity) {
        entity.addEffect(new MobEffectInstance(HaemaContent.ContentMobEffects.INCOMPATIBLE_BLOOD, 600));
    }

    private InjectionResult tryInject(LivingEntity entity) {
        if (entity.getLevel().isClientSide()) {
            return InjectionResult.NOTHING;
        }

        var component = VampireComponent.KEY.getNullable(entity);
        if (component == null) {
            return InjectionResult.NOTHING;
        }

        if (component.isVampire()) {
            component.addBlood(this.quality.multiplier * BloodApi.dropletsToBloodUnits(HaemaContent.ContentConstants.INJECTOR_CAPACITY_DROPLETS));
            return InjectionResult.INJECTED;
        }

        if (!this.quality.vampiric) {
            return InjectionResult.NOTHING;
        }

        var source = entity.getLevel().registryAccess().registry(VampirismSource.REGISTRY_KEY)
                .flatMap(r -> r.getOptional(HaemaContent.ContentVampirismSources.BLOOD_INJECTOR));
        if (source.isEmpty()) {
            return InjectionResult.NOTHING;
        }

        if (component.tryConvert(source.get())) {
            return InjectionResult.CONVERTED;
        } else {
            return InjectionResult.INCOMPATIBLE;
        }
    }

    @Override
    public void appendHoverText(ItemStack itemStack, @Nullable Level level, List<Component> tooltip, TooltipFlag tooltipFlag) {
        super.appendHoverText(itemStack, level, tooltip, tooltipFlag);
        tooltip.add(Component.translatable(DESCRIPTION_TRANSLATION_KEY, this.quality.text()).withStyle(ChatFormatting.GRAY));
    }

    //TODO dispenser behaviour

    enum InjectionResult {
        NOTHING,
        INJECTED,
        CONVERTED,
        INCOMPATIBLE
    }
}