package com.williambl.haema.vampiremobs

import com.williambl.haema.effect.SunlightSicknessEffect
import com.williambl.haema.util.HaemaGameRules
import net.minecraft.entity.ai.goal.EscapeSunlightGoal
import net.minecraft.entity.mob.PathAwareEntity

class VampireEscapeSunlightGoal(mob: PathAwareEntity, speed: Double) : EscapeSunlightGoal(mob, speed) {
    override fun canStart(): Boolean {
        return if (mob.target != null) {
            false
        } else if (!mob.world.isDay) {
            false
        } else if (!mob.hasStatusEffect(SunlightSicknessEffect.instance)) {
            false
        } else if (!mob.world.isSkyVisible(mob.blockPos)) {
            false
        } else if (!mob.world.gameRules.getBoolean(HaemaGameRules.vampiresBurn)) {
            false
        } else {
            targetShadedPos()
        }
    }
}