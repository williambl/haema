package com.williambl.haema.api.content.blood;

import com.mojang.datafixers.util.Pair;
import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodFluid;
import net.fabricmc.fabric.api.lookup.v1.entity.EntityApiLookup;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.storage.Storage;
import net.fabricmc.fabric.api.transfer.v1.transaction.Transaction;
import net.minecraft.tags.TagKey;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;

import java.util.Optional;

import static com.williambl.haema.Haema.id;

public class BloodApi {
    public static final EntityApiLookup<Storage<FluidVariant>, Void> ENTITY_BLOOD_STORAGE =
            EntityApiLookup.get(id("blood_storage"), Storage.asClass(), Void.class);

    public static TagKey<EntityType<?>> getEntityTag(BloodQuality quality) {
        return HaemaContent.ContentTags.ENTITY_BLOOD_QUALITY_TAGS.get(quality);
    }

    public static TagKey<Fluid> getFluidTag(BloodQuality quality) {
        return HaemaContent.ContentTags.BLOOD_TAGS.get(quality);
    }

    public static TagKey<Fluid> getFluidTagMinimumQuality(BloodQuality quality) {
        return HaemaContent.ContentTags.MINIMUM_QUALITY_BLOOD_TAGS.get(quality);
    }

    public static double dropletsToBloodUnits(long droplets) {
        return droplets * HaemaContent.ContentConstants.BLOOD_UNITS_PER_DROPLET;
    }

    public static long bloodUnitsToDroplets(double bloodUnits) {
        return (long) Math.ceil(bloodUnits / HaemaContent.ContentConstants.BLOOD_UNITS_PER_DROPLET);
    }

    public static Fluid getFluid(BloodQuality quality) {
        return HaemaContent.ContentFluids.BLOOD.get(quality);
    }

    public static Optional<BloodQuality> getBloodQuality(Fluid fluid) {
        if (fluid instanceof BloodFluid bloodFluid) {
            return Optional.of(bloodFluid.getQuality());
        } else {
            return Optional.empty();
        }
    }

    public static Optional<BloodQuality> getBloodQuality(Entity entity) {
        Optional<BloodQuality> result = Optional.empty();
        for (var entry : HaemaContent.ContentTags.ENTITY_BLOOD_QUALITY_TAGS.entrySet()) {
            if (entity.getType().is(entry.getValue())) {
                result = Optional.of(entry.getKey());
                break;
            }
        }

        return EntityBloodQualityCallback.EVENT.invoker().getBloodQuality(entity, result).getObject();
    }

    public static Optional<Storage<FluidVariant>> getBloodStorage(Entity entity) {
        return Optional.ofNullable(ENTITY_BLOOD_STORAGE.find(entity, null));
    }

    public static Pair<Fluid, Long> extractBlood(Entity entity, long amountInDroplets) {
        var quality = getBloodQuality(entity);
        if (quality.isEmpty()) {
            return Pair.of(Fluids.EMPTY, 0L);
        }

        var fluid = getFluid(quality.get());
        var storage = ENTITY_BLOOD_STORAGE.find(entity, null);
        if (storage == null) {
            return Pair.of(Fluids.EMPTY, 0L);
        }
        try (Transaction tx = Transaction.openOuter()) {
            var res = Pair.of(fluid, storage.extract(FluidVariant.of(fluid), amountInDroplets, tx));
            tx.commit();
            return res;
        }
    }
}
