package com.williambl.haema.vampire;

import dev.onyxstudios.cca.api.v3.component.Component;
import dev.onyxstudios.cca.api.v3.component.ComponentKey;
import dev.onyxstudios.cca.api.v3.component.ComponentRegistry;

import static com.williambl.haema.Haema.id;

/**
 * Handles basic vampire state for an entity.
 */
public interface VampireComponent extends Component {
    /**
     * Get whether the entity is a vampire.
     * @return whether the entity is a vampire
     */
    boolean isVampire();

    /**
     * Set whether the entity is a vampire.
     * @param vampire whether the entity is a vampire
     */
    void setVampire(boolean vampire);

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
}
