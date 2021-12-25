package com.williambl.haema.ability.component

import com.williambl.haema.id
import dev.onyxstudios.cca.api.v3.component.ComponentKey
import dev.onyxstudios.cca.api.v3.component.ComponentRegistryV3
import dev.onyxstudios.cca.api.v3.component.ComponentV3
import dev.onyxstudios.cca.api.v3.component.tick.ServerTickingComponent

interface InvisibilityAbilityComponent: ComponentV3, ServerTickingComponent {
    val invisTicks: Long

    companion object {
        val entityKey: ComponentKey<InvisibilityAbilityComponent> = ComponentRegistryV3.INSTANCE.getOrCreate(id("invisibility"), InvisibilityAbilityComponent::class.java)
    }
}