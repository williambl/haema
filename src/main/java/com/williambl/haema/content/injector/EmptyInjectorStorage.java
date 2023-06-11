package com.williambl.haema.content.injector;

import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodFluid;
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

/**
 * Based on {@link net.fabricmc.fabric.impl.transfer.fluid.EmptyBucketStorage}
 */
@SuppressWarnings("UnstableApiUsage")
public class EmptyInjectorStorage implements InsertionOnlyStorage<FluidVariant> {
	private final ContainerItemContext context;
	private final List<StorageView<FluidVariant>> blankView = List.of(new BlankVariantView<>(FluidVariant.blank(), HaemaContent.Config.INJECTOR_CAPACITY_DROPLETS));

	public EmptyInjectorStorage(ContainerItemContext context) {
		this.context = context;
	}

	@Override
	public long insert(FluidVariant resource, long maxAmount, TransactionContext transaction) {
		StoragePreconditions.notBlankNotNegative(resource, maxAmount);

		if (!context.getItemVariant().isOf(HaemaContent.Items.EMPTY_INJECTOR) || !(resource.getFluid() instanceof BloodFluid blood)) {
			return 0;
		}

		Item fullInjector = HaemaContent.Items.INJECTORS.get(blood.getQuality());

		// Make sure the resource is a correct fluid mapping: the fluid <-> bucket mapping must be bidirectional.
		if (maxAmount >= HaemaContent.Config.INJECTOR_CAPACITY_DROPLETS) {
			ItemVariant newVariant = ItemVariant.of(fullInjector, context.getItemVariant().getNbt());

			if (context.exchange(newVariant, 1, transaction) == 1) {
				return HaemaContent.Config.INJECTOR_CAPACITY_DROPLETS;
			}
		}

		return 0;
	}

	@Override
	public Iterator<StorageView<FluidVariant>> iterator() {
		return blankView.iterator();
	}
}