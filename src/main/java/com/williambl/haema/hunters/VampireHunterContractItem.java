package com.williambl.haema.hunters;

import com.mojang.authlib.GameProfile;
import com.williambl.haema.api.vampire.VampireApi;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;

public class VampireHunterContractItem extends Item {
    private static final String FULFILLED_KEY = "ContractFulfilled";
    private static final String TARGET_KEY = "ContractTarget";

    public static final String FULFILLED_TRANSLATION_KEY = "item.haema.vampire_hunter_contract.fulfilled";
    public static final String UNFULFILLED_TRANSLATION_KEY = "item.haema.vampire_hunter_contract.unfulfilled";
    public static final String TARGET_TRANSLATION_KEY = "item.haema.vampire_hunter_contract.target";
    public static final String NO_TARGET_TRANSLATION_KEY = "item.haema.vampire_hunter_contract.no_target";

    public VampireHunterContractItem(Properties properties) {
        super(properties);
    }

    @Override
    public void appendHoverText(ItemStack itemStack, @Nullable Level level, List<Component> list, TooltipFlag tooltipFlag) {
        if (isFulfilled(itemStack)) {
            list.add(Component.translatable(FULFILLED_TRANSLATION_KEY).withStyle(ChatFormatting.AQUA, ChatFormatting.ITALIC));
        } else {
            list.add(Component.translatable(UNFULFILLED_TRANSLATION_KEY).withStyle(ChatFormatting.GRAY, ChatFormatting.ITALIC));
        }
        list.add(getContractTarget(itemStack).map(target -> Component.translatable(TARGET_TRANSLATION_KEY, target.getName()))
                .orElseGet(() -> Component.translatable(NO_TARGET_TRANSLATION_KEY)));
        super.appendHoverText(itemStack, level, list, tooltipFlag);
    }

    @Override
    public boolean isFoil(ItemStack itemStack) {
        return super.isFoil(itemStack) || isFulfilled(itemStack);
    }

    public static ItemStack create(Level level) {
        if (level.getRandom().nextDouble() < 0.3) {
            return createWithRandomTarget(level);
        }
        return HaemaHunters.HunterItems.VAMPIRE_HUNTER_CONTRACT.getDefaultInstance();
    }

    public static ItemStack createWithRandomTarget(Level level) {
        var stack = HaemaHunters.HunterItems.VAMPIRE_HUNTER_CONTRACT.getDefaultInstance();
        var targets = level.players().stream().filter(VampireApi::isVampire).toList();
        if (targets.size() > 1) {
            var target = targets.get(level.getRandom().nextInt());
            setContractTarget(stack, target);
        }

        return stack;
    }

    public static ItemStack createWithTarget(Player target) {
        var stack = HaemaHunters.HunterItems.VAMPIRE_HUNTER_CONTRACT.getDefaultInstance();
        setContractTarget(stack, target);
        return stack;
    }

    public static boolean isFulfilled(ItemStack stack) {
        return stack.getOrCreateTag().getBoolean(FULFILLED_KEY);
    }

    public static void fulfilContract(ItemStack stack, Entity holder) {
        stack.getOrCreateTag().putBoolean(FULFILLED_KEY, true);
        if (holder instanceof ServerPlayer sP) {
            sP.playNotifySound(SoundEvents.UI_TOAST_CHALLENGE_COMPLETE, SoundSource.NEUTRAL, 1f, 1f);
        }
    }

    public static Optional<GameProfile> getContractTarget(ItemStack contract) {
        return Optional.ofNullable(NbtUtils.readGameProfile(contract.getOrCreateTag().getCompound(TARGET_KEY)));
    }

    public static void setContractTarget(ItemStack contract, Player target) {
        contract.getOrCreateTag().put(TARGET_KEY, NbtUtils.writeGameProfile(new CompoundTag(), target.getGameProfile()));
    }
}
