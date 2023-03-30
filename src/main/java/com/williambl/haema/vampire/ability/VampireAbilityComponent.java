package com.williambl.haema.vampire.ability;

import dev.onyxstudios.cca.api.v3.component.Component;

import java.util.List;
import java.util.Set;

public interface VampireAbilityComponent extends Component {
    Set<VampireAbility> getAbilities();
    void addAbility(VampireAbility ability);
    void removeAbility(VampireAbility ability);
    boolean hasAbility(VampireAbility ability);
}
