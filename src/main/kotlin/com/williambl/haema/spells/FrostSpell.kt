package com.williambl.haema.spells

import net.minecraft.entity.LivingEntity
import net.minecraft.particle.ParticleTypes
import net.minecraft.server.world.ServerWorld
import net.minecraft.world.World

class FrostSpell: Spell() {
    override val chargeTime: Int = 60

    override fun createChargeParticles(world: World, user: LivingEntity, ticksRemaining: Int) {
        val progress = (chargeTime - ticksRemaining).toDouble()/chargeTime.toDouble()

        for (i in 0..20) {
            world.addParticle(
                ParticleTypes.SNOWFLAKE,
                user.x + world.random.nextGaussian() * (0.1 + progress) * user.width/2.0,
                user.getBodyY(progress),
                user.z + world.random.nextGaussian() * (0.1 + progress) * user.width/2.0,
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
                user.x + world.random.nextGaussian() * user.width/2.0,
                user.randomBodyY,
                user.z + world.random.nextGaussian() * user.width/2.0,
                0.0,
                0.5,
                0.0
            )
        }

        if (world is ServerWorld) {
        }
    }
}