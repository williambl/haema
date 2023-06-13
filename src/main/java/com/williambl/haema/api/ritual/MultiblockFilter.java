package com.williambl.haema.api.ritual;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContext;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.HaemaUtil;
import net.minecraft.core.BlockPos;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;

/**
 * A multiblock filter is used to identity multiblock structures. Multiblock filters are defined with a pattern,
 * much like recipes.
 * <p>A multiblock filter's 'focus' is where checking starts from. For a {@link RitualArae}, this would be the altar
 * block.</p>
 * <p>The pattern's innermost array is X, the middle array is Z, and the outermost array is Y.</p>
 * @param pattern           the block pattern
 * @param focusCharacter    the character in the pattern that represents the focus
 * @param predicates        the block predicates for each character in the pattern
 * @param overallPredicate  the predicate that must be true overall for the multiblock to be identified
 * @param focusPosFilterSpace the position of the focus in the filter space (indices into pattern)
 */
public record MultiblockFilter(char[][][] pattern, char focusCharacter, Map<Character, DFunction<Boolean>> predicates, DFunction<Boolean> overallPredicate, BlockPos focusPosFilterSpace) {
    public static final Codec<MultiblockFilter> CODEC = RecordCodecBuilder.create(instance -> instance.group(
            Codec.STRING.listOf().listOf().xmap(
                    l -> l.stream().map(l2 -> l2.stream().map(String::toCharArray).toArray(char[][]::new)).toArray(char[][][]::new),
                    l -> Arrays.stream(l).map(l2 -> Arrays.stream(l2).map(String::new).toList()).toList()
            ).comapFlatMap(HaemaUtil::ensureIsCuboid, Function.identity()).fieldOf("pattern").forGetter(MultiblockFilter::pattern),
            HaemaUtil.CHARACTER_CODEC.fieldOf("focus").forGetter(MultiblockFilter::focusCharacter),
            Codec.unboundedMap(HaemaUtil.CHARACTER_CODEC, DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(HaemaDFunctions.BLOCK_IN_WORLD), Function.identity())).fieldOf("predicates").forGetter(MultiblockFilter::predicates),
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(HaemaDFunctions.BLOCK_IN_WORLD), Function.identity()).fieldOf("overall_predicate").forGetter(MultiblockFilter::overallPredicate)
    ).apply(instance, MultiblockFilter::new));

    public MultiblockFilter(char[][][] pattern, char focusCharacter, Map<Character, DFunction<Boolean>> predicates, DFunction<Boolean> overallPredicate) {
        this(pattern, focusCharacter, predicates, overallPredicate, focusPos(pattern, focusCharacter));
    }

    private static BlockPos focusPos(char[][][] pattern, char focusCharacter) {
        for (int y = 0; y < pattern.length; y++) {
            for (int z = 0; z < pattern[y].length; z++) {
                for (int x = 0; x < pattern[y][z].length; x++) {
                    if (pattern[y][z][x] == focusCharacter) {
                        return new BlockPos(x, y, z);
                    }
                }
            }
        }

        throw new IllegalStateException("No focus character found in pattern");
    }

    public boolean matches(int checkXFocusSpace, int checkYFocusSpace, int checkZFocusSpace, DFContext blockInWorldDFContext) {
        int checkXFilterSpace = checkXFocusSpace + this.focusPosFilterSpace.getX();
        int checkYFilterSpace = checkYFocusSpace + this.focusPosFilterSpace.getY();
        int checkZFilterSpace = checkZFocusSpace + this.focusPosFilterSpace.getZ();

        if (checkXFilterSpace < 0 || checkYFilterSpace < 0 || checkZFilterSpace < 0) {
            return true;
        }

        if (checkYFilterSpace >= this.pattern.length) {
            return true;
        }
        char[][] ySlice = this.pattern[checkYFilterSpace];
        if (checkZFilterSpace >= ySlice.length) {
            return true;
        }
        char[] zSlice = ySlice[checkZFilterSpace];
        if (checkXFilterSpace >= zSlice.length) {
            return true;
        }
        char c = zSlice[checkXFilterSpace];
        if (c == ' ') {
            return true;
        }

        return this.predicates.get(c).apply(blockInWorldDFContext);
    }
}
