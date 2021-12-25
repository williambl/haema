package com.williambl.haema.compat.rats

import com.williambl.haema.component.VampirePlayerComponent
import com.williambl.haema.convert
import com.williambl.haema.damagesource.BloodLossDamageSource
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.isVampirable
import com.williambl.haema.isVampire
import ladysnake.ratsmischief.common.entity.RatEntity
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.goal.MeleeAttackGoal
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.world.ServerWorld

class VampiRatAttackGoal(private val actor: RatEntity, speed: Double, pauseWhenMobIdle: Boolean) : MeleeAttackGoal(actor, speed, pauseWhenMobIdle) {
    override fun canStart(): Boolean = super.canStart() && hasValidTarget() && actor.isVampire

    override fun shouldContinue(): Boolean = super.shouldContinue() && hasValidTarget() && actor.isVampire

    private fun hasValidTarget(): Boolean = actor.target != null && actor.target!!.isAlive && (!actor.target!!.isVampire || ((!actor.target!!.isVampirable() && actor.target!!.isBloodSource()) && !actor.hasStatusEffect(VampiricStrengthEffect.instance)))

    private fun LivingEntity.isBloodSource(): Boolean {
        return this.type.run { isIn(VampirePlayerComponent.goodBloodTag) || isIn(VampirePlayerComponent.mediumBloodTag) || isIn(VampirePlayerComponent.poorBloodTag) }
    }

    override fun tick() {
        if (actor.target == null) stop()
        super.tick()
    }

    override fun attack(target: LivingEntity?, squaredDistance: Double) {
        if (getSquaredMaxAttackDistance(target) < squaredDistance) return
        if (target == null) return

        resetCooldown()
        target.damage(BloodLossDamageSource.instance, 0.4f)

        (actor.world as ServerWorld).spawnParticles(
            DustParticleEffect.DEFAULT,
            target.x,
            target.y,
            target.z,
            10,
            0.2,
            0.2,
            0.2,
            0.5
        )

        if (target.isVampirable() && !(target).isVampire) {
            if (target is PlayerEntity) {
                if (actor.world.gameRules.getBoolean(ratsCanConvertPlayers)) {
                    convert(target)
                }
            } else {
                target.isVampire = true
            }
        } else if (!target.isVampirable()) {
           actor.addStatusEffect(StatusEffectInstance(VampiricStrengthEffect.instance, 200, 2))
        }
    }
}
