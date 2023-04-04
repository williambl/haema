package com.williambl.haema.vampire;

import com.williambl.haema.Haema;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.LivingEntity;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class EntityVampireComponent implements VampireComponent {
    private double blood;
    private @Nullable VampirismSource vampirismSource;
    private final @NotNull LivingEntity entity;

    private static final String VAMPIRISM_SOURCE_KEY = "vampirism_source";
    private static final String BLOOD_KEY = "blood";

    public EntityVampireComponent(@NotNull LivingEntity entity) {
        this.entity = entity;
    }

    @Override
    public boolean isVampire() {
        return this.vampirismSource != null;
    }

    @Override
    public @Nullable VampirismSource getVampirismSource() {
        return this.vampirismSource;
    }

    @Override
    public boolean tryConvert(VampirismSource source) {
        if (this.vampirismSource != null) {
            return false;
        }

        this.vampirismSource = source;
        return true;
    }

    @Override
    public boolean tryCure(VampirismSource source) {
        if (this.vampirismSource == null || !(this.vampirismSource.canBeCuredBy(this.entity.level.registryAccess(), source))) {
            return false;
        }

        this.vampirismSource = null;
        return true;
    }

    @Override
    public double getBlood() {
        return this.blood;
    }

    @Override
    public void setBlood(double blood) {
        this.blood = blood;
    }

    @Override
    public void readFromNbt(CompoundTag tag) {
        var registries = this.entity.level.registryAccess();
        var sourceRegistry = registries.registryOrThrow(VampirismSource.REGISTRY_KEY);
        if (tag.contains(VAMPIRISM_SOURCE_KEY, Tag.TAG_STRING)) {
            String keyString = tag.getString(VAMPIRISM_SOURCE_KEY);
            if (ResourceLocation.isValidResourceLocation(keyString)) {
                ResourceKey<VampirismSource> sourceKey = ResourceKey.create(VampirismSource.REGISTRY_KEY, new ResourceLocation(keyString));
                var source = sourceRegistry.get(sourceKey);
                if (source != null) {
                    this.vampirismSource = source;
                } else {
                    Haema.LOGGER.warn("Nonexistent Vampirism Source {} when decoding vampire component for {}", keyString, this.entity.getScoreboardName());
                }
            } else {
                Haema.LOGGER.warn("Invalid Vampirism Source key {} when decoding vampire component for {}", keyString, this.entity.getScoreboardName());
            }
        }

        if (tag.contains(BLOOD_KEY, Tag.TAG_DOUBLE)) {
            this.blood = tag.getDouble(BLOOD_KEY);
        }
    }

    @Override
    public void writeToNbt(CompoundTag tag) {
        var registries = this.entity.level.registryAccess();
        var sourceRegistry = registries.registryOrThrow(VampirismSource.REGISTRY_KEY);
        if (this.vampirismSource != null) {
            var resourceLocation = sourceRegistry.getKey(vampirismSource);
            if (resourceLocation != null) {
                tag.putString(VAMPIRISM_SOURCE_KEY, resourceLocation.toString());
            } else {
                Haema.LOGGER.warn("Vampirism Source with no key {} when encoding vampire component for {}", this.vampirismSource, this.entity.getScoreboardName());
            }
        }

        tag.putDouble(BLOOD_KEY, this.blood);
    }
}
