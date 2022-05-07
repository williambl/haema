package com.williambl.haema.vampiremobs

import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.goal.TrackTargetGoal
import net.minecraft.entity.mob.MobEntity
import net.minecraft.entity.mob.PathAwareEntity
import java.util.*
import kotlin.random.asKotlinRandom

class TargetOwnersTargetGoal<T>(private val actor: T) : TrackTargetGoal(actor, false) where T: PathAwareEntity, T: OwnedMob {
    init {
        controls = EnumSet.of(Control.TARGET)
    }

    override fun canStart(): Boolean {
        val owner = actor.owner
        return owner.targets.isNotEmpty()
    }

    override fun start() {
        this.actor.target = this.actor.owner.targets.filter(this.actor::canTarget).random(this.actor.random.asKotlinRandom())
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
}