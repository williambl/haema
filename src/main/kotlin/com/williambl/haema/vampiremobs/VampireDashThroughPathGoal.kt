package com.williambl.haema.vampiremobs

import net.minecraft.entity.mob.PathAwareEntity
import net.minecraft.util.math.Vec3d
import kotlin.math.min

class VampireDashThroughPathGoal(actor: PathAwareEntity, private val minimumDashDistance: Double, maxDashDistance: Double) : VampireDashGoal(actor, maxDashDistance) {
    private val sqrMinDashDistance: Double = this.minimumDashDistance * this.minimumDashDistance
    var dashTarget: Vec3d? = null

    override fun canStart(): Boolean {
        this.dashTarget = null
        if (!super.canStart()) {
            return false
        }

        this.dashTarget = this.dashTarget()
        return this.dashTarget != null
    }

    override fun dashTarget(): Vec3d? {
        return this.actor.navigation.currentPath?.let {
            if (it.length == 0) {
                return@let null
            }

            var nodePos: Vec3d? = null
            for (i in it.length-1 downTo 0) {
                nodePos = it.getNode(i).pos
                val dist = this.actor.squaredDistanceTo(nodePos)

                if (dist < this.sqrMinDashDistance) {
                    nodePos = null
                    break
                }

                if (dist <= this.sqrMaxDashDistance) {
                    break
                }


                if (i == 0) {
                    nodePos = null
                }
            }

            return@let nodePos
        }
    }
}