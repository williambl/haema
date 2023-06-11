package com.williambl.haema.content.blood;

import net.fabricmc.fabric.api.transfer.v1.context.ContainerItemContext;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.item.ItemVariant;
import net.fabricmc.fabric.api.transfer.v1.storage.StoragePreconditions;
import net.fabricmc.fabric.api.transfer.v1.storage.StorageView;
import net.fabricmc.fabric.api.transfer.v1.storage.base.BlankVariantView;
import net.fabricmc.fabric.api.transfer.v1.storage.base.InsertionOnlyStorage;
import net.fabricmc.fabric.api.transfer.v1.transaction.TransactionContext;
import net.minecraft.world.item.Item;

import java.util.Iterator;
import java.util.List;
import java.util.function.Function;

/**
 * Based on {@link net.fabricmc.fabric.impl.transfer.fluid.EmptyBucketStorage}
 */
@SuppressWarnings("UnstableApiUsage")
public class EmptyBloodStorage implements InsertionOnlyStorage<FluidVariant> {
	private final ContainerItemContext context;
	private final long capacity;
	private final Item empty;
	private final Function<BloodQuality, Item> full;
	private final List<StorageView<FluidVariant>> blankView;

	public EmptyBloodStorage(ContainerItemContext context, long capacity, Item empty, Function<BloodQuality, Item> full) {
		this.context = context;
		this.capacity = capacity;
		this.empty = empty;
		this.full = full;
		this.blankView = List.of(new BlankVariantView<>(FluidVariant.blank(), capacity));
	}

	@Override
	public long insert(FluidVariant resource, long maxAmount, TransactionContext transaction) {
		StoragePreconditions.notBlankNotNegative(resource, maxAmount);

		if (!context.getItemVariant().isOf(this.empty) || !(resource.getFluid() instanceof BloodFluid blood)) {
			return 0;
		}

		Item fullItem = this.full.apply(blood.getQuality());

		if (maxAmount >= this.capacity) {
			ItemVariant newVariant = ItemVariant.of(fullItem, context.getItemVariant().getNbt());

			if (context.exchange(newVariant, 1, transaction) == 1) {
				return this.capacity;
			}
		}

		return 0;
	}

	@Override
	public Iterator<StorageView<FluidVariant>> iterator() {
		return blankView.iterator();
	}
}