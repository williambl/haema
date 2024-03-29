package com.williambl.haema.component

import com.williambl.haema.ability.VampireAbility
import com.williambl.haema.id
import dev.onyxstudios.cca.api.v3.component.ComponentKey
import dev.onyxstudios.cca.api.v3.component.ComponentRegistryV3
import dev.onyxstudios.cca.api.v3.component.ComponentV3
import dev.onyxstudios.cca.api.v3.component.tick.ServerTickingComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.util.ActionResult
import net.minecraft.util.Identifier

interface VampireComponent : ComponentV3, ServerTickingComponent {

    var isVampire: Boolean
    var isPermanentVampire: Boolean
    var isKilled: Boolean

    var absoluteBlood: Double
    val blood: Double
    val lastFed: Long

    var abilities: MutableMap<VampireAbility, Int>
    var ritualsUsed: MutableSet<Identifier>

    fun removeBlood(blood: Double)
    fun addBlood(blood: Double)
    fun feed(entity: LivingEntity): ActionResult

    companion object {
        val entityKey: ComponentKey<VampireComponent> = ComponentRegistryV3.INSTANCE.getOrCreate(id("vampire"), VampireComponent::class.java)
    }
}
