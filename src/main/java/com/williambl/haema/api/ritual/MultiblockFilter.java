package com.williambl.haema.api.ritual;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.HaemaUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.state.pattern.BlockInWorld;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * A multiblock filter is used to identity multiblock structures. Multiblock filters are defined with a pattern,
 * much like recipes.
 * <p>A multiblock filter's 'focus' is where checking starts from. For a {@link RitualArae}, this would be the altar
 * block.</p>
 * @param pattern           the block pattern
 * @param focusCharacter    the character in the pattern that represents the focus
 * @param predicates        the block predicates for each character in the pattern
 * @param overallPredicate  the predicate that must be true overall for the multiblock to be identified
 */
public record MultiblockFilter(char[][][] pattern, char focusCharacter, Map<Character, DFunction<Boolean>> predicates, DFunction<Boolean> overallPredicate) {
    public static final Codec<MultiblockFilter> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.STRING.listOf().listOf().xmap(
                    l -> l.stream().map(l2 -> l2.stream().map(String::toCharArray).toArray(char[][]::new)).toArray(char[][][]::new),
                    l -> Arrays.stream(l).map(l2 -> Arrays.stream(l2).map(String::new).toList()).toList()
            ).fieldOf("pattern").forGetter(MultiblockFilter::pattern),
            HaemaUtil.CHARACTER_CODEC.fieldOf("focus").forGetter(MultiblockFilter::focusCharacter),
            Codec.unboundedMap(HaemaUtil.CHARACTER_CODEC, DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(HaemaDFunctions.BLOCK_IN_WORLD), Function.identity())).fieldOf("predicates").forGetter(MultiblockFilter::predicates),
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(HaemaDFunctions.BLOCK_IN_WORLD), Function.identity()).fieldOf("overall_predicate").forGetter(MultiblockFilter::overallPredicate)
    ).apply(instance, MultiblockFilter::new));

    //TODO rotation maybe?
    public Predicate<BlockInWorld> createPredicate() {
        final BlockPos focusPos = focusPos();
        return focus -> {
            final var level = focus.getLevel();
            final BlockPos start = focus.getPos().offset(focusPos.multiply(-1));
            for (int x = 0; x < pattern.length; x++) {
                for (int y = 0; y < pattern[x].length; y++) {
                    for (int z = 0; z < pattern[x][y].length; z++) {
                        final char c = pattern[x][y][z];
                        if (c != ' ') {
                            final BlockPos pos = start.offset(x, y, z);
                            if (predicates.containsKey(c)) {
                                if (!predicates.get(c).apply(HaemaDFunctions.blockInWorld(new BlockInWorld(level, pos, true)))) {
                                    return false;
                                }
                            } else {
                                return false;
                            }
                        }
                    }
                }
            }

            return true; //TODO: check overall predicate
        };
    }

    private BlockPos focusPos() {
        for (int x = 0; x < pattern.length; x++) {
            for (int y = 0; y < pattern[x].length; y++) {
                for (int z = 0; z < pattern[x][y].length; z++) {
                    if (pattern[x][y][z] == focusCharacter) {
                        return new BlockPos(x, y, z);
                    }
                }
            }
        }

        throw new IllegalStateException("No focus character found in pattern");
    }
}
