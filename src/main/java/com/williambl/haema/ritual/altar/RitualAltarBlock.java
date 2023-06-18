package com.williambl.haema.ritual.altar;

import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.api.ritual.ritual.RitualContainer;
import com.williambl.haema.ritual.HaemaRituals;
import com.williambl.haema.ritual.ritual.RightClickRitualTrigger;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BaseEntityBlock;
import net.minecraft.world.level.block.RenderShape;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityTicker;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.pattern.BlockInWorld;
import net.minecraft.world.phys.BlockHitResult;
import org.jetbrains.annotations.Nullable;

public class RitualAltarBlock extends BaseEntityBlock {
    public RitualAltarBlock(Properties properties) {
        super(properties);
    }

    @Override
    public InteractionResult use(BlockState blockState, Level level, BlockPos blockPos, Player player, InteractionHand interactionHand, BlockHitResult blockHitResult) {
        var be = (RitualAltarBlockEntity) level.getBlockEntity(blockPos);
        if (be == null) {
            return InteractionResult.PASS;
        }

        var araeKey = be.arae();
        if (araeKey.isEmpty()) {
            return InteractionResult.PASS;
        }

        var arae = level.registryAccess().registryOrThrow(RitualArae.REGISTRY_KEY).get(araeKey.get());
        if (arae == null) {
            return InteractionResult.PASS;
        }

        //TODO move a lot of this stuff into API
        var items = arae.getItemsInFluidSpaces(level, blockPos);
        var fluid = arae.getFluid(level, blockPos);
        var container = new RitualContainer(items, fluid, blockPos, arae, player);
        var blockInWorld = new BlockInWorld(level, blockPos, true);
        var itemHeld = player.getItemInHand(interactionHand);
        var rituals = level.getRecipeManager().getAllRecipesFor(HaemaRituals.RitualRecipeTypes.RITUAL);
        for (var ritual : rituals) {
            if (!(ritual.data().trigger() instanceof RightClickRitualTrigger trigger)) {
                continue;
            }

            if (!trigger.predicate().apply(HaemaDFunctions.entityInteractWithBlock(player, itemHeld, blockInWorld))) {
                continue;
            }

            if (!ritual.matches(container, level)) {
                continue;
            }

            ritual.assemble(container, level.registryAccess());
            return InteractionResult.SUCCESS;
        }

        return InteractionResult.PASS;
    }

    @Override
    public RenderShape getRenderShape(BlockState blockState) {
        return RenderShape.MODEL;
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos blockPos, BlockState blockState) {
        return new RitualAltarBlockEntity(blockPos, blockState);
    }

    @Nullable
    @Override
    public <T extends BlockEntity> BlockEntityTicker<T> getTicker(Level level, BlockState blockState, BlockEntityType<T> blockEntityType) {
        return createTickerHelper(blockEntityType, HaemaRituals.RitualBlockEntities.RITUAL_ALTAR, RitualAltarBlockEntity::tick);
    }
}
