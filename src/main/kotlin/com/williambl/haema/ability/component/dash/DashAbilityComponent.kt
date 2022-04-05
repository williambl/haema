package com.williambl.haema.ability.component.dash

import com.williambl.haema.id
import dev.onyxstudios.cca.api.v3.component.ComponentKey
import dev.onyxstudios.cca.api.v3.component.ComponentRegistryV3
import dev.onyxstudios.cca.api.v3.component.ComponentV3

interface DashAbilityComponent: ComponentV3 {
    val lastDashed: Long

    fun canDash(): Boolean
    fun dash()

    fun updateDashCooldown(newValue: Int)

    companion object {
        val entityKey: ComponentKey<DashAbilityComponent> = ComponentRegistryV3.INSTANCE.getOrCreate(id("dash"), DashAbilityComponent::class.java)
    }
}