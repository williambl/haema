package com.williambl.haema.api

import net.fabricmc.fabric.api.event.Event
import net.fabricmc.fabric.api.event.EventFactory.createArrayBacked
import net.fabricmc.fabric.api.util.TriState
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.world.World

object VampireBurningEvents {
    public val TRIGGER: Event<Trigger> = createArrayBacked(Trigger::class.java) { listeners -> Trigger { player, world ->
        var isAnyTrue = false
        for (listener in listeners) {
            if (listener.willVampireBurn(player, world) == TriState.TRUE)
                isAnyTrue = true
        }
        TriState.of(isAnyTrue)
    }}

    public val VETO: Event<Veto> = createArrayBacked(Veto::class.java) { listeners -> Veto { player, world ->
        for (listener in listeners.sortedByDescending { it.getPriority() }) {
            if (listener.willVampireBurn(player, world) == TriState.FALSE)
                return@Veto TriState.FALSE
        }
        TriState.TRUE
    }}

    fun interface Trigger {
        /**
         * If nothing returns `true`, no burning occurs.
         */
        fun willVampireBurn(player: PlayerEntity, world: World): TriState
    }

    fun interface Veto {
        /**
         * Higher priority listeners are run first.
         * If you want to return `false` make sure your priority is set right:
         *
         *  - [Int.MAX_VALUE] - fail because it's nighttime, or it's raining, or the player is creative
         *  - `50` - fail because of some weird burning-suppression field or something that the player is standing in
         *  - `20` - fail because the player has an umbrella over their head
         *  - `10` - fail because the player has protective clothing
         *
         *  Listeners with equal priority are done in registration order.
         */
        fun getPriority(): Int = Int.MAX_VALUE

        fun willVampireBurn(player: PlayerEntity, world: World): TriState
    }
}
