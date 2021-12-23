/*
 * Copyright (c) 2021 C4
 *
 * This file is part of Somnus, a mod made for Minecraft.
 *
 * Somnus is free software: you can redistribute it and/or modify it under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation, either version 3 of
 * the License, or any later version.
 *
 * Somnus is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with Somnus.
 * If not, see <https://www.gnu.org/licenses/>.
 */

package com.williambl.haema.api

import com.williambl.haema.api.WorldSleepEvents.WorldWakeTime
import net.fabricmc.fabric.api.event.EventFactory
import net.minecraft.server.world.ServerWorld

/**
 * Provides world events related to sleeping.
 */
object WorldSleepEvents {
    /**
     * Called when players finish sleeping and the world needs to update the time of day.
     *
     *
     * Called once in [ServerWorld.tick]
     */
    val WORLD_WAKE_TIME = EventFactory.createArrayBacked(WorldWakeTime::class.java) { listeners ->
        WorldWakeTime { serverWorld: ServerWorld, newTime: Long, curTime: Long ->
            var time = newTime
            for (listener in listeners) {
                time = listener.getWorldWakeTime(serverWorld, time, curTime)
            }
            time
        }
    }

    fun interface WorldWakeTime {
        /**
         * @param world   The server world
         * @param newTime The new time of day for the world
         * @param curTime The current time of day for the world
         * @return The new time of day for the world, overriding the previous new time
         */
        fun getWorldWakeTime(world: ServerWorld, newTime: Long, curTime: Long): Long
    }
}