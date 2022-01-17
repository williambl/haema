package com.williambl.haema.client

import net.fabricmc.api.EnvType
import net.fabricmc.api.Environment
import net.minecraft.client.particle.*
import net.minecraft.client.world.ClientWorld
import net.minecraft.particle.DefaultParticleType
import net.minecraft.util.math.MathHelper
import kotlin.math.max

@Environment(EnvType.CLIENT)
class MistParticle internal constructor(
    world: ClientWorld?,
    x: Double,
    y: Double,
    z: Double,
    velocityX: Double,
    velocityY: Double,
    velocityZ: Double,
    spriteProvider: SpriteProvider
) : SpriteBillboardParticle(world, x, y, z, 0.0, 0.0, 0.0) {
    private val spriteProvider: SpriteProvider

    init {
        velocityMultiplier = 0.96f
        this.spriteProvider = spriteProvider
        this.velocityX *= 0.10000000149011612
        this.velocityY *= 0.10000000149011612
        this.velocityZ *= 0.10000000149011612
        this.velocityX += velocityX
        this.velocityY += velocityY
        this.velocityZ += velocityZ
        val g = 0.8f - (Math.random() * 0.3).toFloat()
        colorRed = g
        colorGreen = g
        colorBlue = g
        scale *= 0.875f
        val i = (8.0 / (Math.random() * 0.8 + 0.3)).toInt()
        maxAge = max(i.toFloat() * 5.0f, 2.5f).toInt()
        collidesWithWorld = false
        setSpriteForAge(spriteProvider)
    }

    override fun getType(): ParticleTextureSheet {
        return ParticleTextureSheet.PARTICLE_SHEET_TRANSLUCENT
    }

    override fun getSize(tickDelta: Float): Float {
        return scale * MathHelper.clamp((age.toFloat() + tickDelta) / maxAge.toFloat() * 32.0f, 0.0f, 1.0f)
    }

    override fun tick() {
        super.tick()
        if (!dead) {
            setSpriteForAge(spriteProvider)
            val playerEntity = world.getClosestPlayer(x, y, z, 2.0, false)
            if (playerEntity != null) {
                val d = playerEntity.y
                if (y > d) {
                    y += (d - y) * 0.2
                    velocityY += (playerEntity.velocity.y - velocityY) * 0.2
                    setPos(x, y, z)
                }
            }
        }
    }

    @Environment(EnvType.CLIENT)
    class Factory(private val spriteProvider: SpriteProvider) : ParticleFactory<DefaultParticleType> {
        override fun createParticle(
            defaultParticleType: DefaultParticleType,
            clientWorld: ClientWorld,
            d: Double,
            e: Double,
            f: Double,
            g: Double,
            h: Double,
            i: Double
        ): Particle {
            return MistParticle(clientWorld, d, e, f, g, h, i, spriteProvider)
        }
    }
}