package com.williambl.haema.content;

import net.minecraft.MethodsReturnNonnullByDefault;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.particles.ParticleOptions;
import net.minecraft.core.particles.ParticleTypes;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.tags.FluidTags;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.LiquidBlock;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.material.FlowingFluid;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.FluidState;
import org.jetbrains.annotations.Nullable;

import java.util.Optional;

@MethodsReturnNonnullByDefault
public abstract class BloodFluid extends FlowingFluid {
	public static final float MIN_LEVEL_CUTOFF = 0.44444445F;

	@Override
	public Fluid getFlowing() {
		return HaemaContent.Fluids.FLOWING_BLOOD;
	}

	@Override
	public Fluid getSource() {
		return HaemaContent.Fluids.BLOOD;
	}

	@Override
	public Item getBucket() {
		return Items.AIR;
	}

	@Override
	public void animateTick(Level level, BlockPos blockPos, FluidState fluidState, RandomSource randomSource) {
	}

	@Override
	public void randomTick(Level level, BlockPos blockPos, FluidState fluidState, RandomSource randomSource) {
	}

	@Nullable
	@Override
	public ParticleOptions getDripParticle() {
		return ParticleTypes.DRIPPING_WATER;
	}

	@Override
	protected void beforeDestroyingBlock(LevelAccessor levelAccessor, BlockPos blockPos, BlockState blockState) {
		BlockEntity blockEntity = blockState.hasBlockEntity() ? levelAccessor.getBlockEntity(blockPos) : null;
		Block.dropResources(blockState, levelAccessor, blockPos, blockEntity);
	}

	@Override
	public int getSlopeFindDistance(LevelReader levelReader) {
		return 2;
	}

	@Override
	public BlockState createLegacyBlock(FluidState fluidState) {
		return HaemaContent.Fluids.BLOOD_BLOCK.defaultBlockState().setValue(LiquidBlock.LEVEL, getLegacyLevel(fluidState));
	}

	@Override
	public boolean isSame(Fluid fluid) {
		return fluid == HaemaContent.Fluids.BLOOD || fluid == HaemaContent.Fluids.FLOWING_BLOOD;
	}

	@Override
	public int getDropOff(LevelReader levelReader) {
		return 2;
	}

	@Override
	public boolean canBeReplacedWith(FluidState fluidState, BlockGetter blockGetter, BlockPos blockPos, Fluid fluid, Direction direction) {
		return fluidState.getHeight(blockGetter, blockPos) >= 0.44444445F && fluid.is(FluidTags.WATER);
	}

	@Override
	public int getTickDelay(LevelReader levelReader) {
		return 20;
	}

	@Override
	public int getSpreadDelay(Level level, BlockPos blockPos, FluidState fluidState, FluidState fluidState2) {
		int i = this.getTickDelay(level);
		if (!fluidState.isEmpty()
			&& !fluidState2.isEmpty()
			&& !fluidState.getValue(FALLING)
			&& !fluidState2.getValue(FALLING)
			&& fluidState2.getHeight(level, blockPos) > fluidState.getHeight(level, blockPos)
			&& level.getRandom().nextInt(4) != 0) {
			i *= 4;
		}

		return i;
	}

	@Override
	protected boolean canConvertToSource(Level level) {
		return false;
	}

	@Override
	protected boolean isRandomlyTicking() {
		return true;
	}

	@Override
	protected float getExplosionResistance() {
		return 100.0F;
	}

	@Override
	public Optional<SoundEvent> getPickupSound() {
		return Optional.of(SoundEvents.BUCKET_FILL_LAVA);
	}

	public static class Flowing extends BloodFluid {
		@Override
		protected void createFluidStateDefinition(StateDefinition.Builder<Fluid, FluidState> builder) {
			super.createFluidStateDefinition(builder);
			builder.add(LEVEL);
		}

		@Override
		public int getAmount(FluidState fluidState) {
			return fluidState.getValue(LEVEL);
		}

		@Override
		public boolean isSource(FluidState fluidState) {
			return false;
		}
	}

	public static class Source extends BloodFluid {
		@Override
		public int getAmount(FluidState fluidState) {
			return 8;
		}

		@Override
		public boolean isSource(FluidState fluidState) {
			return true;
		}
	}
}
