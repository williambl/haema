package com.williambl.haema.vampiremobs

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.goal.TrackTargetGoal
import net.minecraft.entity.mob.MobEntity
import net.minecraft.entity.mob.PathAwareEntity
import net.minecraft.util.math.random.Random
import java.util.*

class TargetOwnersTargetGoal<T>(private val actor: T) : TrackTargetGoal(actor, false) where T: PathAwareEntity, T: OwnedMob {
    init {
        controls = EnumSet.of(Control.TARGET)
    }

    override fun canStart(): Boolean {
        val owner = actor.owner
        return owner.targets.isNotEmpty()
    }

    override fun start() {
        this.actor.target = this.actor.owner.targets.filter(this.actor::canTarget).randomOrNull(this.actor.random) ?: return
        super.start()
    }

    private val LivingEntity?.targets: Set<LivingEntity>
        get() {
            return when (this) {
                is MultiTargeter -> {
                    this.targets
                }
                is MobEntity -> {
                    this.target?.let { setOf(it) } ?: setOf()
                }
                else -> {
                    setOf()
                }
            }
        }

    private fun <T> Collection<T>.randomOrNull(random: Random): T? {
        if (isEmpty()) {
            return null
        }
        return elementAt(random.nextInt(size))
    }
}