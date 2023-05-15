package com.williambl.haema.api.vampire.ability;

import dev.onyxstudios.cca.api.v3.component.Component;
import dev.onyxstudios.cca.api.v3.component.ComponentKey;
import dev.onyxstudios.cca.api.v3.component.ComponentRegistry;
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent;
import dev.onyxstudios.cca.api.v3.component.tick.CommonTickingComponent;

import java.util.List;
import java.util.Set;

import static com.williambl.haema.Haema.id;

public interface VampireAbilitiesComponent extends Component, CommonTickingComponent, AutoSyncedComponent {
    Set<VampireAbility> getAbilities();
    void addAbility(VampireAbility ability);
    void removeAbility(VampireAbility ability);
    boolean hasAbility(VampireAbility ability);
    <T extends VampireAbilityPower> List<T> getPowersOfClass(Class<T> clazz);

    ComponentKey<VampireAbilitiesComponent> KEY = ComponentRegistry.getOrCreate(id("vampire_abilities"), VampireAbilitiesComponent.class);
}
