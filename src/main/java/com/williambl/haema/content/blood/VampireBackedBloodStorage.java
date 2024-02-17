package com.williambl.haema.content.blood;

import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.api.vampire.VampireComponent;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.storage.StoragePreconditions;
import net.fabricmc.fabric.api.transfer.v1.storage.base.SingleSlotStorage;
import net.fabricmc.fabric.api.transfer.v1.transaction.TransactionContext;
import net.fabricmc.fabric.api.transfer.v1.transaction.base.SnapshotParticipant;
import net.minecraft.world.entity.LivingEntity;

public class VampireBackedBloodStorage extends SnapshotParticipant<Double> implements SingleSlotStorage<FluidVariant> {
    private final BloodQuality entityBloodQuality;
    private final VampireComponent component;
    private final double maxBlood;
    private double bloodLevel;

    public VampireBackedBloodStorage(VampireComponent component, BloodQuality entityBloodQuality) {
        this.entityBloodQuality = entityBloodQuality;
        this.component = component;
        this.bloodLevel = component.getBlood();
        this.maxBlood = VampireComponent.MAX_BLOOD;
    }

    @Override
    public long insert(FluidVariant insertedVariant, long maxAmount, TransactionContext transaction) {
        StoragePreconditions.notBlankNotNegative(insertedVariant, maxAmount);
        var insertedQuality = BloodApi.getBloodQuality(insertedVariant.getFluid());

        // we'll accept all blood, but w/ multiplier
        if (insertedQuality.isPresent()) {
            long insertedAmount = Math.min(maxAmount, this.getCapacity() - BloodApi.bloodUnitsToDroplets(this.bloodLevel));

            if (insertedAmount > 0) {
                this.updateSnapshots(transaction);

                this.bloodLevel += BloodApi.dropletsToBloodUnits(insertedAmount) * insertedQuality.get().multiplier;

                return insertedAmount;
            }
        }

        return 0;
    }

    @Override
    public long extract(FluidVariant extractedVariant, long maxAmount, TransactionContext transaction) {
        StoragePreconditions.notBlankNotNegative(extractedVariant, maxAmount);

        // can only take out the exact type of blood we have
        if (extractedVariant.equals(this.getResource())) {
            long extractedAmount = Math.min(maxAmount, BloodApi.bloodUnitsToDroplets(this.bloodLevel));

            if (extractedAmount > 0) {
                this.updateSnapshots(transaction);
                this.bloodLevel -= BloodApi.dropletsToBloodUnits(extractedAmount);

                return extractedAmount;
            }
        }

        return 0;
    }

    @Override
    public boolean isResourceBlank() {
        return false;
    }

    @Override
    public FluidVariant getResource() {
        return FluidVariant.of(BloodApi.getFluid(this.entityBloodQuality));
    }

    @Override
    public long getAmount() {
        return BloodApi.bloodUnitsToDroplets(this.bloodLevel);
    }

    @Override
    public long getCapacity() {
        return BloodApi.bloodUnitsToDroplets(this.maxBlood);
    }

    @Override
    protected Double createSnapshot() {
        return this.bloodLevel;
    }

    @Override
    protected void readSnapshot(Double snapshot) {
        this.bloodLevel = snapshot;
    }

    @Override
    protected void onFinalCommit() {
        this.component.setBlood(this.bloodLevel);
    }
}
