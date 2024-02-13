package com.williambl.haema.api.vampire.ability;

import dev.onyxstudios.cca.api.v3.component.Component;
import dev.onyxstudios.cca.api.v3.component.ComponentKey;
import dev.onyxstudios.cca.api.v3.component.ComponentRegistry;
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent;
import dev.onyxstudios.cca.api.v3.component.tick.CommonTickingComponent;
import net.minecraft.core.Holder;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import static com.williambl.haema.Haema.id;

public interface VampireAbilitiesComponent extends Component, CommonTickingComponent, AutoSyncedComponent {
    Set<Holder.Reference<VampireAbility>> getAbilities();
    Set<Holder.Reference<VampireAbility>> getEnabledAbilities();
    boolean isAbilityEnabled(Holder<VampireAbility> ability);
    boolean addAbility(Holder<VampireAbility> ability);
    boolean removeAbility(Holder<VampireAbility> ability);
    boolean hasAbility(Holder<VampireAbility> ability);
    boolean setActiveAbility(Holder<VampireAbility> ability);
    Optional<Holder<VampireAbility>> getActiveAbility();
    <T extends VampireAbilityPower> List<T> getEnabledPowersOfClass(Class<T> clazz);

    ComponentKey<VampireAbilitiesComponent> KEY = ComponentRegistry.getOrCreate(id("vampire_abilities"), VampireAbilitiesComponent.class);
}
