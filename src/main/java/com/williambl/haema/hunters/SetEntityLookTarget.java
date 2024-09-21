package com.williambl.haema.hunters;

import com.mojang.datafixers.util.Pair;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.behavior.EntityTracker;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.ai.memory.MemoryStatus;
import net.tslat.smartbrainlib.api.core.behaviour.ExtendedBehaviour;
import net.tslat.smartbrainlib.util.BrainUtils;

import java.util.List;
import java.util.function.Predicate;

/**
 * Set the {@link MemoryModuleType#LOOK_TARGET} of the brain owner from {@link MemoryModuleType#NEAREST_VISIBLE_LIVING_ENTITIES}
 * @param <E> The entity
 */
public class SetEntityLookTarget<E extends LivingEntity> extends ExtendedBehaviour<E> {
	private static final List<Pair<MemoryModuleType<?>, MemoryStatus>> MEMORY_REQUIREMENTS = ObjectArrayList.of(
			Pair.of(MemoryModuleType.LOOK_TARGET, MemoryStatus.VALUE_ABSENT), Pair.of(MemoryModuleType.NEAREST_VISIBLE_LIVING_ENTITIES, MemoryStatus.VALUE_PRESENT)
	);

	protected Predicate<LivingEntity> predicate = e -> true;

	protected LivingEntity target = null;

	/**
	 * Set the predicate for the player to look at.
	 * @param predicate The predicate
	 * @return this
	 */
	public SetEntityLookTarget<E> predicate(Predicate<LivingEntity> predicate) {
		this.predicate = predicate;
		return this;
	}

	@Override
	protected List<Pair<MemoryModuleType<?>, MemoryStatus>> getMemoryRequirements() {
		return MEMORY_REQUIREMENTS;
	}

	@Override
	protected boolean checkExtraStartConditions(ServerLevel level, E entity) {
		this.target = BrainUtils.getMemory(entity, MemoryModuleType.NEAREST_VISIBLE_LIVING_ENTITIES).findClosest(this.predicate).orElse(null);
		return this.target != null;
	}

	@Override
	protected void start(E entity) {
		BrainUtils.setMemory(entity, MemoryModuleType.LOOK_TARGET, new EntityTracker(this.target, true));
	}

	@Override
	protected void stop(E entity) {
		this.target = null;
	}
}
