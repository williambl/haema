package com.williambl.haema.mixin;

import com.williambl.haema.ability.component.mist_form.MistFormAbilityComponent;
import net.minecraft.block.*;
import net.minecraft.entity.Entity;
import net.minecraft.entity.LivingEntity;
import net.minecraft.tag.TagKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.shape.VoxelShape;
import net.minecraft.util.shape.VoxelShapes;
import net.minecraft.world.BlockView;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import static com.williambl.haema.HaemaKt.id;

@Mixin(AbstractBlock.AbstractBlockState.class)
public abstract class AbstractBlockStateMixin {
    @Shadow public abstract Block getBlock();
    @Shadow protected abstract BlockState asBlockState();

    @Shadow public abstract boolean isIn(TagKey<Block> tag);

    @Unique private static final TagKey<Block> PERMEABLE_BLOCKS = TagKey.of(Registry.BLOCK_KEY, id("mist_permeable"));

    @Inject(at = @At("HEAD"), method = "getCollisionShape(Lnet/minecraft/world/BlockView;Lnet/minecraft/util/math/BlockPos;Lnet/minecraft/block/ShapeContext;)Lnet/minecraft/util/shape/VoxelShape;", cancellable = true)
    private void phaseThroughBlocks(BlockView world, BlockPos pos, ShapeContext context, CallbackInfoReturnable<VoxelShape> cir) {
        VoxelShape blockShape = getBlock().getCollisionShape(asBlockState(), world, pos, context);
        if(!blockShape.isEmpty() && context instanceof EntityShapeContext esc) {
            if(esc.getEntity() instanceof LivingEntity livingEntity) {
                if (
                        isIn(PERMEABLE_BLOCKS) &&
                        MistFormAbilityComponent.Companion.getEntityKey().isProvidedBy(livingEntity) &&
                                MistFormAbilityComponent.Companion.getEntityKey().get(livingEntity).isInMistForm()
                ) {
                    cir.setReturnValue(VoxelShapes.empty());
                }
            }
        }
    }

    @Inject(method = "onEntityCollision", at = @At("HEAD"), cancellable = true)
    private void preventCollisionWhenPhasing(World world, BlockPos pos, Entity entity, CallbackInfo ci) {
        if(entity instanceof LivingEntity livingEntity) {
            if (
                    MistFormAbilityComponent.Companion.getEntityKey().isProvidedBy(livingEntity) &&
                            MistFormAbilityComponent.Companion.getEntityKey().get(livingEntity).isInMistForm()
            ) {
                ci.cancel();
            }
        }
    }
}
