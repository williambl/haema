package com.williambl.haema.compat.origins

import com.williambl.haema.VampireBloodManager
import com.williambl.haema.api.VampireBurningEvents
import io.github.apace100.apoli.component.PowerHolderComponent
import io.github.apace100.apoli.power.PhasingPower
import io.github.apace100.apoli.power.Power
import io.github.apace100.apoli.power.PowerType
import io.github.apace100.apoli.power.factory.PowerFactory
import io.github.apace100.apoli.registry.ApoliRegistries
import io.github.apace100.calio.data.SerializableData
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.entity.LivingEntity
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry
import java.util.function.BiFunction

fun registerPowerTypes() {
    val id = Identifier("haema:vampire")
    val powerFactory: PowerFactory<Power?>? = PowerFactory(id, SerializableData()) {
        BiFunction<PowerType<Power?>, LivingEntity?, Power?> { powerType, player -> VampirePower(powerType, player) }
    }
    Registry.register<PowerFactory<*>, PowerFactory<Power?>>(
        ApoliRegistries.POWER_FACTORY,
        id,
        powerFactory
    )
}

fun registerOriginsCompatEvents() {
    VampireBurningEvents.VETO.register(VampireBurningEvents.Veto { player, _ ->
        val phasingPowers = PowerHolderComponent.getPowers(player, PhasingPower::class.java)
        if (phasingPowers.isNotEmpty() && phasingPowers.any { it.isActive }) {
            (player.hungerManager as VampireBloodManager).removeBlood(0.001)
            TriState.FALSE
        } else TriState.DEFAULT
    })
}
