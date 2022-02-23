package com.williambl.haema.ability.component.mist_form

import com.williambl.haema.id
import dev.onyxstudios.cca.api.v3.component.ComponentKey
import dev.onyxstudios.cca.api.v3.component.ComponentRegistryV3
import dev.onyxstudios.cca.api.v3.component.ComponentV3
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import dev.onyxstudios.cca.api.v3.component.tick.ClientTickingComponent
import dev.onyxstudios.cca.api.v3.component.tick.ServerTickingComponent

interface MistFormAbilityComponent: ComponentV3, AutoSyncedComponent, ServerTickingComponent, ClientTickingComponent {
    var isInMistForm: Boolean
    fun toggleMistForm() {
        this.isInMistForm = !this.isInMistForm
    }
    fun shouldRenderAsFullMistForm(): Boolean

    fun expandMist()
    fun isMistExpanded(): Boolean
    fun canExpandMist(): Boolean

    companion object {
        val entityKey: ComponentKey<MistFormAbilityComponent> = ComponentRegistryV3.INSTANCE.getOrCreate(id("mist_form"), MistFormAbilityComponent::class.java)
    }
}