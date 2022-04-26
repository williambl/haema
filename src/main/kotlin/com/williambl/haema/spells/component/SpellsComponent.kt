package com.williambl.haema.spells.component

import com.williambl.haema.id
import com.williambl.haema.spells.SpellInstance
import dev.onyxstudios.cca.api.v3.component.ComponentKey
import dev.onyxstudios.cca.api.v3.component.ComponentRegistryV3
import dev.onyxstudios.cca.api.v3.component.ComponentV3
import dev.onyxstudios.cca.api.v3.component.tick.CommonTickingComponent

interface SpellsComponent: ComponentV3, CommonTickingComponent {
    val spells: List<SpellInstance>

    fun addSpell(instance: SpellInstance)
    fun removeSpell(instance: SpellInstance)
    fun clearSpells()

    companion object {
        val entityKey: ComponentKey<SpellsComponent> = ComponentRegistryV3.INSTANCE.getOrCreate(id("spells"), SpellsComponent::class.java)
    }
}