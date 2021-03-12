package com.williambl.haema.compat.origins

import com.williambl.haema.VampireBloodManager
import com.williambl.haema.api.VampireBurningEvents
import io.github.apace100.origins.component.OriginComponent
import io.github.apace100.origins.power.PhasingPower
import io.github.apace100.origins.power.Power
import io.github.apace100.origins.power.PowerType
import io.github.apace100.origins.power.factory.PowerFactory
import io.github.apace100.origins.registry.ModRegistries
import io.github.apace100.origins.util.SerializableData
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry
import java.util.function.BiFunction

fun registerPowerTypes() {
    val id = Identifier("haema:vampire")
    val powerFactory: PowerFactory<Power?>? = PowerFactory(id, SerializableData()) {
        BiFunction<PowerType<Power?>, PlayerEntity?, Power?> { powerType, player -> VampirePower(powerType, player) }
    }
    Registry.register<PowerFactory<*>, PowerFactory<Power?>>(
        ModRegistries.POWER_FACTORY,
        id,
        powerFactory
    )
}

fun registerOriginsCompatEvents() {
    VampireBurningEvents.VETO.register(VampireBurningEvents.Veto { player, _ ->
        if (OriginComponent.hasPower(player, PhasingPower::class.java)) {
            (player.hungerManager as VampireBloodManager).removeBlood(0.05)
            TriState.FALSE
        } else TriState.DEFAULT
    })
}
