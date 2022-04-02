package com.williambl.haema.client

import com.williambl.haema.ability.AbilityModule
import net.minecraft.client.MinecraftClient
import net.minecraft.client.render.RenderLayer
import net.minecraft.client.render.VertexConsumer
import net.minecraft.client.render.VertexConsumerProvider
import net.minecraft.client.render.WorldRenderer
import net.minecraft.particle.DefaultParticleType
import net.minecraft.util.math.Vec3d
import java.util.*

@Suppress("UnstableApiUsage")
class ParticleVertexConsumer(val particleType: DefaultParticleType, val worldRenderer: WorldRenderer, val random: Random): VertexConsumer {
    val positions = mutableListOf<Vec3d>()

    override fun vertex(x: Double, y: Double, z: Double): VertexConsumer {
        positions.add(Vec3d(x, y, z))
        if (positions.size > 2) {
            val firstPos = positions[positions.size-1]
            val secondPos = positions[positions.size-2]
            val thirdPos = positions.last()
            for (i in 0..10) {
                val pos = firstPos.add(secondPos.subtract(firstPos).multiply(random.nextDouble())).add(thirdPos.subtract(firstPos).multiply(random.nextDouble()))
                worldRenderer.addParticle(particleType, true, pos.x, pos.y, pos.z, 0.0, 0.0, 0.0)
            }
        }
        worldRenderer.addParticle(particleType, true, x, y, z, 0.0, 0.0, 0.0)
        return this
    }

    override fun color(red: Int, green: Int, blue: Int, alpha: Int): VertexConsumer {
        return this // noop
    }

    override fun texture(u: Float, v: Float): VertexConsumer {
        return this // noop
    }

    override fun overlay(u: Int, v: Int): VertexConsumer {
        return this // noop
    }

    override fun light(u: Int, v: Int): VertexConsumer {
        return this // noop
    }

    override fun normal(x: Float, y: Float, z: Float): VertexConsumer {
        return this // noop
    }

    override fun next() {
        return // noop
    }

    override fun fixedColor(red: Int, green: Int, blue: Int, alpha: Int) {
        return // noop
    }

    override fun unfixColor() {
        return // noop
    }

    class Provider: VertexConsumerProvider {
        override fun getBuffer(layer: RenderLayer): VertexConsumer {
            return ParticleVertexConsumer(AbilityModule.MIST_PARTICLE, MinecraftClient.getInstance().worldRenderer, Random())
        }
    }
}