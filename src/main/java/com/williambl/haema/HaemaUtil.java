package com.williambl.haema;

import com.mojang.serialization.Codec;
import com.mojang.serialization.DataResult;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.storage.Storage;
import net.fabricmc.fabric.api.transfer.v1.storage.StorageView;
import net.fabricmc.fabric.api.transfer.v1.transaction.Transaction;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.stats.Stats;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.*;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.LayeredCauldronBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.gameevent.GameEvent;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
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

    public static final Codec<Character> CHARACTER_CODEC = Codec.STRING.comapFlatMap(
            s -> {
                if (s.length() != 1) {
                    return DataResult.error(() -> "String must be one character long, got %d characters (%s)".formatted(s.length(), s));
                }
                return DataResult.success(s.charAt(0));
            },
            Object::toString);

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

    public static DataResult<char[][][]> ensureIsCuboid(char[][][] toCheck) {
        return isCuboid(toCheck) ? DataResult.success(toCheck) : DataResult.error(() -> "Array %s is jagged".formatted((Object) toCheck));
    }

    public static boolean isCuboid(char[][][] toCheck) {
        int sizeB, sizeC;
        sizeB = toCheck[0].length;
        sizeC = toCheck[0][0].length;
        for (char[][] charsB : toCheck) {
            if (charsB.length != sizeB) {
                return false;
            }
            for (char[] charsC : charsB) {
                if (charsC.length != sizeC) {
                    return false;
                }
            }
        }

        return true;
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

    public static @Nullable List<ItemStack> allMatchOne(Iterable<ItemStack> stacks, Collection<Ingredient> ingredients) {
        List<ItemStack> removed = new ArrayList<>();
        outer: for (var ingredient : ingredients) {
            for (var iter = stacks.iterator(); iter.hasNext();) {
                var item = iter.next();
                if (ingredient.test(item)) {
                    removed.add(item.split(1));
                    if (item.isEmpty()) {
                        iter.remove();
                    }
                    continue outer;
                }
            }
            return null;
        }

        return removed;
    }

    // from fabric-api's FluidStorageUtil
    public static boolean moveFluid(Storage<FluidVariant> from, Storage<FluidVariant> to) {
        for (StorageView<FluidVariant> view : from) {
            if (view.isResourceBlank()) continue;
            FluidVariant resource = view.getResource();
            long maxExtracted;

            // check how much can be extracted
            try (Transaction extractionTestTransaction = Transaction.openOuter()) {
                maxExtracted = view.extract(resource, Long.MAX_VALUE, extractionTestTransaction);
                extractionTestTransaction.abort();
            }

            try (Transaction transferTransaction = Transaction.openOuter()) {
                // check how much can be inserted
                long accepted = to.insert(resource, maxExtracted, transferTransaction);

                // extract it, or rollback if the amounts don't match
                if (accepted > 0 && view.extract(resource, accepted, transferTransaction) == accepted) {
                    transferTransaction.commit();

                    return true;
                }
            }
        }

        return false;
    }
}
