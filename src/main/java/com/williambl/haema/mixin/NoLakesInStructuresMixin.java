package com.williambl.haema.mixin;

import com.williambl.haema.hunter.structure.StructuresKt;
import net.minecraft.util.math.ChunkSectionPos;
import net.minecraft.world.gen.feature.LakeFeature;
import net.minecraft.world.gen.feature.SingleStateFeatureConfig;
import net.minecraft.world.gen.feature.StructureFeature;
import net.minecraft.world.gen.feature.util.FeatureContext;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.List;

/**
 * Stops lakes from spawning in Haema's structures.
 *
 * <p>From <a href="https://github.com/TelepathicGrunt/RepurposedStructures/blob/1.16/src/main/java/com/telepathicgrunt/repurposedstructures/mixin/features/NoLakesInStructuresMixin.java">Repurposed Structures</a>.</p>
 *
 * @author TelepathicGrunt
 */
@Mixin(LakeFeature.class)
public class NoLakesInStructuresMixin {
    @Inject(
            method = "generate",
            at = @At(value = "INVOKE_ASSIGN", target = "Lnet/minecraft/util/math/BlockPos;down(I)Lnet/minecraft/util/math/BlockPos;"),
            cancellable = true
    )
    private void repurposedstructures_noLakesInStructures(FeatureContext<SingleStateFeatureConfig> context, CallbackInfoReturnable<Boolean> cir) {
        ChunkSectionPos chunkPos = ChunkSectionPos.from(context.getOrigin());
        for (StructureFeature<?> structure : List.of(StructuresKt.getSmallVampireHunterOutpostFeature(), StructuresKt.getVampireHunterOutpostFeature())) {
            if (context.getWorld().getStructures(chunkPos, structure).findAny().isPresent()) {
                cir.setReturnValue(false);
            }
        }
    }
}
