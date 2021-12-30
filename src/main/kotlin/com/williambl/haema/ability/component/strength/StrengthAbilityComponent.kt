package com.williambl.haema.ability.component.strength

import com.williambl.haema.id
import dev.onyxstudios.cca.api.v3.component.ComponentKey
import dev.onyxstudios.cca.api.v3.component.ComponentRegistryV3
import dev.onyxstudios.cca.api.v3.component.ComponentV3
import dev.onyxstudios.cca.api.v3.component.tick.ServerTickingComponent

interface StrengthAbilityComponent: ComponentV3, ServerTickingComponent {
    companion object {
        val entityKey: ComponentKey<StrengthAbilityComponent> = ComponentRegistryV3.INSTANCE.getOrCreate(id("strength"), StrengthAbilityComponent::class.java)
    }
}