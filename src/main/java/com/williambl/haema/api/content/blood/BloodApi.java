package com.williambl.haema.api.content.blood;

import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodFluid;
import net.minecraft.core.registries.Registries;
import net.minecraft.tags.TagKey;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;

import java.util.Arrays;
import java.util.EnumMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.williambl.haema.Haema.id;

public class BloodApi {
    private static final Map<BloodQuality, TagKey<EntityType<?>>> ENTITY_BLOOD_QUALITY_TAGS = new EnumMap<>(Arrays.stream(BloodQuality.values()).collect(Collectors.toMap(
            Function.identity(),
            quality -> TagKey.create(Registries.ENTITY_TYPE, id("blood_quality/" + quality.getSerializedName())))));

    public static TagKey<EntityType<?>> getEntityTag(BloodQuality quality) {
        return ENTITY_BLOOD_QUALITY_TAGS.get(quality);
    }

    public static TagKey<Fluid> getFluidTag(BloodQuality quality) {
        return HaemaContent.ContentTags.BLOOD_TAGS.get(quality);
    }

    public static TagKey<Fluid> getFluidTagMinimumQuality(BloodQuality quality) {
        return HaemaContent.ContentTags.MINIMUM_QUALITY_BLOOD_TAGS.get(quality);
    }

    public static double dropletsToBloodUnits(long droplets) {
        return droplets * HaemaContent.Config.BLOOD_UNITS_PER_DROPLET;
    }

    public static long bloodUnitsToDroplets(double bloodUnits) {
        return (long) Math.ceil(bloodUnits / HaemaContent.Config.BLOOD_UNITS_PER_DROPLET);
    }

    public static Fluid getFluid(BloodQuality quality) {
        return HaemaContent.Fluids.BLOOD.get(quality);
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
        for (var entry : ENTITY_BLOOD_QUALITY_TAGS.entrySet()) {
            if (entity.getType().is(entry.getValue())) {
                result = Optional.of(entry.getKey());
                break;
            }
        }

        return EntityBloodQualityCallback.EVENT.invoker().getBloodQuality(entity, result).getObject();
    }

    public static Fluid extractBlood(Entity entity, long amountInDroplets) {
        var quality = getBloodQuality(entity);
        if (quality.isEmpty()) {
            return Fluids.EMPTY;
        }

        boolean extracted = EntityBloodExtractionCallback.EVENT.invoker().extractBlood(entity, quality.get(), amountInDroplets);
        if (!extracted) {
            return Fluids.EMPTY;
        }

        return getFluid(quality.get());
    }
}
