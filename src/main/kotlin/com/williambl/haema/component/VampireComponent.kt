package com.williambl.haema.component

import com.williambl.haema.VampireAbility
import dev.onyxstudios.cca.api.v3.component.ComponentKey
import dev.onyxstudios.cca.api.v3.component.ComponentRegistryV3
import dev.onyxstudios.cca.api.v3.component.ComponentV3
import net.minecraft.util.Identifier

interface VampireComponent : ComponentV3 {
    var isVampire: Boolean
    var isPermanentVampire: Boolean
    var isKilled: Boolean

    var abilities: MutableMap<VampireAbility, Int>

    companion object {
        val entityKey: ComponentKey<VampireComponent> = ComponentRegistryV3.INSTANCE.getOrCreate(Identifier("haema:vampire"), VampireComponent::class.java)
    }
}