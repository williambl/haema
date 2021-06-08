package com.williambl.haema.compat.rats

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.damagesource.BloodLossDamageSource
import com.williambl.haema.effect.VampiricStrengthEffect
import ladysnake.ratsmischief.common.entity.RatEntity
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.goal.MeleeAttackGoal
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.world.ServerWorld

class VampiRatAttackGoal(private val actor: RatEntity, speed: Double, pauseWhenMobIdle: Boolean) : MeleeAttackGoal(actor, speed, pauseWhenMobIdle) {
    override fun canStart(): Boolean = super.canStart() && hasValidTarget() && actor.isVampire()

    override fun shouldContinue(): Boolean = super.shouldContinue() && hasValidTarget() && actor.isVampire()

    private fun hasValidTarget(): Boolean = actor.target != null && actor.target!!.isAlive && (!actor.target!!.isVampire() || ((actor.target !is Vampirable && actor.target!!.isBloodSource()) && !actor.hasStatusEffect(VampiricStrengthEffect.instance)))

    private fun LivingEntity.isVampire(): Boolean {
        return (this is Vampirable && this.isVampire)
    }

    private fun LivingEntity.isBloodSource(): Boolean {
        return this.type.run { isIn(VampireBloodManager.goodBloodTag) || isIn(VampireBloodManager.mediumBloodTag) || isIn(VampireBloodManager.poorBloodTag) }
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

        if (target is Vampirable && !(target as Vampirable).isVampire) {
            (target as Vampirable).isVampire = true
        } else if (target !is Vampirable) {
           actor.addStatusEffect(StatusEffectInstance(VampiricStrengthEffect.instance, 200, 2))
        }
    }
}
