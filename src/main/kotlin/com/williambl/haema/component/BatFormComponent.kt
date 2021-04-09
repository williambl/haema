package com.williambl.haema.component

import com.williambl.haema.abilities.VampireAbility
import dev.onyxstudios.cca.api.v3.component.ComponentKey
import dev.onyxstudios.cca.api.v3.component.ComponentRegistryV3
import dev.onyxstudios.cca.api.v3.component.ComponentV3
import net.minecraft.util.Identifier

interface BatFormComponent : ComponentV3 {
    var isBat: Boolean

    companion object {
        val entityKey: ComponentKey<BatFormComponent> = ComponentRegistryV3.INSTANCE.getOrCreate(Identifier("haema:bat_form"), BatFormComponent::class.java)
    }
}