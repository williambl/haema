package com.williambl.haema.api.vampire;

import dev.onyxstudios.cca.api.v3.component.Component;
import dev.onyxstudios.cca.api.v3.component.ComponentKey;
import dev.onyxstudios.cca.api.v3.component.ComponentRegistry;
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent;
import org.jetbrains.annotations.Nullable;

import static com.williambl.haema.Haema.id;

/**
 * Handles basic vampire state for an entity.
 */
public interface VampireComponent extends Component, AutoSyncedComponent {
    /**
     * Get whether the entity is a vampire.
     * @return whether the entity is a vampire
     */
    boolean isVampire();

    /**
     * Get the source of the entity's vampirism.
     * @return the source of the entity's vampirism, or null if the entity is not a vampire
     */
    @Nullable VampirismSource getVampirismSource();

    /**
     * Try to convert the entity to a vampire.
     * @param source    the source to convert this entity with
     * @return          whether the entity was successfully converted
     */
    boolean tryConvert(VampirismSource source);

    /**
     * Try to cure the entity of vampirism.
     * @param source    the source to cure this entity with
     * @return          whether the entity was successfully cured
     */
    boolean tryCure(VampirismSource source);

    /**
     * Get the entity's blood level.
     * @return the entity's blood level
     */
    double getBlood();

    /**
     * Set the entity's blood level.
     * @param blood the entity's blood level
     */
    void setBlood(double blood);

    ComponentKey<VampireComponent> KEY = ComponentRegistry.getOrCreate(id("vampire"), VampireComponent.class);
    double MAX_BLOOD = 20;
}
