package com.williambl.haema.content.blood;

import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.content.blood.BloodQuality;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.storage.StoragePreconditions;
import net.fabricmc.fabric.api.transfer.v1.storage.base.SingleSlotStorage;
import net.fabricmc.fabric.api.transfer.v1.transaction.TransactionContext;
import net.fabricmc.fabric.api.transfer.v1.transaction.base.SnapshotParticipant;
import net.minecraft.world.entity.LivingEntity;

public class EntityHealthBackedBloodStorage extends SnapshotParticipant<Double> implements SingleSlotStorage<FluidVariant> {
    private final LivingEntity entity;
    private final BloodQuality entityBloodQuality;
    private final double maxHealth;
    private double health;

    public EntityHealthBackedBloodStorage(LivingEntity entity, BloodQuality entityBloodQuality) {
        this.entity = entity;
        this.entityBloodQuality = entityBloodQuality;
        this.health = entity.getHealth();
        this.maxHealth = this.entity.getMaxHealth();
    }

    @Override
    public long insert(FluidVariant insertedVariant, long maxAmount, TransactionContext transaction) {
        StoragePreconditions.notBlankNotNegative(insertedVariant, maxAmount);
        var insertedQuality = BloodApi.getBloodQuality(insertedVariant.getFluid());

        // we'll accept better blood
        if (insertedQuality.isPresent() && insertedQuality.get().multiplier >= this.entityBloodQuality.multiplier) {
            long insertedAmount = Math.min(maxAmount, this.getCapacity() - BloodApi.bloodUnitsToDroplets(this.health));

            if (insertedAmount > 0) {
                this.updateSnapshots(transaction);

                this.health += BloodApi.dropletsToBloodUnits(insertedAmount);

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
            long extractedAmount = Math.min(maxAmount, BloodApi.bloodUnitsToDroplets(this.health));

            if (extractedAmount > 0) {
                this.updateSnapshots(transaction);
                this.health -= BloodApi.dropletsToBloodUnits(extractedAmount);

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
        return BloodApi.bloodUnitsToDroplets(this.health);
    }

    @Override
    public long getCapacity() {
        return BloodApi.bloodUnitsToDroplets(this.maxHealth);
    }

    @Override
    protected Double createSnapshot() {
        return this.health;
    }

    @Override
    protected void readSnapshot(Double snapshot) {
        this.health = snapshot;
    }

    @Override
    protected void onFinalCommit() {
        if (this.health < this.entity.getHealth()) {
            this.entity.hurt(this.entity.damageSources().magic(), /* TODO custom damage source */ (float) (this.entity.getHealth() - this.health));
        } else if (this.health > this.entity.getHealth()) {
            this.entity.heal((float) (this.health - this.entity.getHealth()));
        }
    }
}
