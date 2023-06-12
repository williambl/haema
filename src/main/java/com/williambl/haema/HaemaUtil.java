package com.williambl.haema;

import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContextSpec;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundSource;
import net.minecraft.stats.Stats;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.*;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.LayeredCauldronBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.gameevent.GameEvent;

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

public class HaemaUtil {
    public static final Codec<AttributeModifier> ATTRIBUTE_MODIFIER_CODEC = CompoundTag.CODEC.comapFlatMap(
            tag -> Optional.ofNullable(AttributeModifier.load(tag))
                    .map(DataResult::success)
                    .orElse(DataResult.error(() -> "Unable to create Attribute Modifier from tag: "+tag)),
            AttributeModifier::save);
    public static final Codec<MobEffectInstance> MOB_EFFECT_INSTANCE_CODEC = CompoundTag.CODEC.comapFlatMap(
            tag -> Optional.ofNullable(MobEffectInstance.load(tag))
                    .map(DataResult::success)
                    .orElse(DataResult.error(() -> "Unable to create MobEffectInstance from tag: "+tag)),
                    instance -> instance.save(new CompoundTag()));

    public static <T> Predicate<ResourceKey<T>> checkInRegistry(Registry<T> registry, String logMessage) {
        return (t) -> {
            if (!registry.containsKey(t)) {
                Haema.LOGGER.warn(logMessage, t.location());
                return false;
            }
            return true;
        };
    }

    public static <T extends Block> T registerWithItem(ResourceLocation id, T block, Item .Properties properties) {
        Registry.register(BuiltInRegistries.BLOCK, id, block);
        Registry.register(BuiltInRegistries.ITEM, id, new BlockItem(block, properties));
        return block;
    }

    public static InteractionResult fillBottle(
            BlockState blockState,
            Level level,
            BlockPos blockPos,
            Player player,
            InteractionHand interactionHand,
            ItemStack itemStack,
            ItemStack itemStack2,
            Predicate<BlockState> predicate,
            SoundEvent soundEvent
    ) {
        if (!predicate.test(blockState)) {
            return InteractionResult.PASS;
        } else {
            if (!level.isClientSide) {
                Item item = itemStack.getItem();
                player.setItemInHand(interactionHand, ItemUtils.createFilledResult(itemStack, player, itemStack2));
                player.awardStat(Stats.USE_CAULDRON);
                player.awardStat(Stats.ITEM_USED.get(item));
                LayeredCauldronBlock.lowerFillLevel(blockState, level, blockPos);
                level.playSound(null, blockPos, soundEvent, SoundSource.BLOCKS, 1.0F, 1.0F);
                level.gameEvent(null, GameEvent.FLUID_PICKUP, blockPos);
            }

            return InteractionResult.sidedSuccess(level.isClientSide);
        }
    }

    public static InteractionResult emptyBottle(
            Level level, BlockPos blockPos, Player player, InteractionHand interactionHand, ItemStack itemStack, BlockState blockState, SoundEvent soundEvent
    ) {
        if (!level.isClientSide) {
            Item item = itemStack.getItem();
            player.setItemInHand(interactionHand, ItemUtils.createFilledResult(itemStack, player, new ItemStack(Items.GLASS_BOTTLE)));
            player.awardStat(Stats.FILL_CAULDRON);
            player.awardStat(Stats.ITEM_USED.get(item));
            level.setBlockAndUpdate(blockPos, blockState);
            level.playSound((Player)null, blockPos, soundEvent, SoundSource.BLOCKS, 1.0F, 1.0F);
            level.gameEvent((Entity)null, GameEvent.FLUID_PLACE, blockPos);
        }

        return InteractionResult.sidedSuccess(level.isClientSide);
    }

    public static <T extends DFunction<?>> Function<T, DataResult<T>> verifyDFunction(DFContextSpec spec) {
        return t -> spec.satisfies(t.getSpec()) ? DataResult.success(t) : DataResult.error(() -> "DFunction spec %s is not satisfied by %s".formatted(spec, t.getSpec()));
    }

    public static boolean isInteger(String s) {
        try {
            Integer.parseInt(s);
            return true;
        } catch (NumberFormatException ignored) {
            return false;
        }
    }

    public static boolean isInteger(String s, int radix) {
        try {
            Integer.parseInt(s, radix);
            return true;
        } catch (NumberFormatException ignored) {
            return false;
        }
    }
}
