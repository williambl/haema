package com.williambl.haema.vampiremobs

import net.minecraft.entity.ai.goal.Goal
import net.minecraft.entity.mob.MobEntity

class SpawnReinforcementsGoal<T>(val mob: T, private val maxReinforcementSpawns: Int, private val cooldown: Int, val condition: () -> Boolean): Goal() where T: MobEntity, T: ReinforcementSpawner {
    private var ticksUntilNextTry: Int = toGoalTicks(this.cooldown)

    override fun canStart(): Boolean {
        if (this.mob.timesSpawnedReinforcements >= this.maxReinforcementSpawns) {
            return false
        }

        if (this.ticksUntilNextTry > 0) {
            this.ticksUntilNextTry--
            return false
        }

        return this.condition()
    }

    override fun start() {
        this.ticksUntilNextTry = toGoalTicks(this.cooldown)
        this.mob.spawnReinforcements()
    }
}