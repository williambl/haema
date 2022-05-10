package com.williambl.haema.spells

import net.minecraft.entity.LivingEntity
import net.minecraft.particle.ParticleTypes
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.math.Vec2f
import net.minecraft.world.World
import kotlin.math.PI
import kotlin.math.cos
import kotlin.math.sin

class FrostSpell: Spell() {
    override val chargeTime: Int = 60

    override fun createChargeParticles(world: World, user: LivingEntity, ticksRemaining: Int) {
        val progress = (chargeTime - ticksRemaining).toDouble() / chargeTime.toDouble()
        val theta = progress * 2 * PI * 5
        val offset = polarToCartesian(Vec2f(user.width, theta.toFloat()))

        for (i in 0..20) {
            world.addParticle(
                ParticleTypes.SNOWFLAKE,
                user.x + offset.x * (if (i % 2 == 0) 1f else -1f) + world.random.nextGaussian() * 0.05,
                user.getBodyY(progress),
                user.z + offset.y * (if (i % 2 == 0) 1f else -1f) + world.random.nextGaussian() * 0.05,
                0.0,
                -0.1,
                0.0
            )
        }
    }

    override fun use(world: World, user: LivingEntity) {
        for (i in 0..50) {
            world.addParticle(
                ParticleTypes.SNOWFLAKE,
                user.x + world.random.nextGaussian() * user.width / 2.0,
                user.randomBodyY,
                user.z + world.random.nextGaussian() * user.width / 2.0,
                0.0,
                0.5,
                0.0
            )
        }

        if (world is ServerWorld) {
        }
    }

    companion object {
        /**
         * Converts a polar vector of (r, theta) to a cartesian vector of (x, y).
         *
         * (joke)Since this is a frost spell and polar environments are frosty, it is appropriate to use polar coordinates in
         * the spellcasting animation. We convert back to cartesian coordinates because Minecraft uses cartesian coordinates
         * (as its worlds are flat planes and do not have poles)(/joke)
         *
         * @param polar the polar vector
         * @return the cartesian vector
         */
        private fun polarToCartesian(polar: Vec2f): Vec2f {
            return Vec2f(polar.x * cos(polar.y), polar.x * sin(polar.y))
        }
    }
}