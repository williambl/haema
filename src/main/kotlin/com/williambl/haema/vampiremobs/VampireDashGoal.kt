package com.williambl.haema.vampiremobs

import com.williambl.haema.ability.component.dash.DashAbilityComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.goal.Goal
import net.minecraft.entity.mob.PathAwareEntity
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Vec3d
import java.util.*

abstract class VampireDashGoal(protected val actor: PathAwareEntity, protected val maxDashDistance: Double): Goal() {
    protected val sqrMaxDashDistance: Double = this.maxDashDistance * this.maxDashDistance
    init {
    }

    override fun canStart(): Boolean {
        return DashAbilityComponent.entityKey.get(this.actor).canDash()
    }

    override fun start() {
        DashAbilityComponent.entityKey.get(this.actor).dash()
    }

    abstract fun dashTarget(): Vec3d?
}