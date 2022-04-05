package com.williambl.haema.spells

import net.minecraft.entity.EntityType
import net.minecraft.entity.passive.BatEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundEvents
import net.minecraft.text.MutableText
import net.minecraft.text.TranslatableText
import net.minecraft.util.Hand
import net.minecraft.util.Util
import net.minecraft.util.math.Vec3d
import net.minecraft.util.math.Vec3f
import net.minecraft.util.registry.Registry
import net.minecraft.world.World

class Spell {
    private val translationKey: String by lazy { Util.createTranslationKey("spell", SpellsModule.SPELL_REGISTRY.getId(this)) }

    fun getName(): MutableText = TranslatableText(translationKey)

    fun use(world: World, user: PlayerEntity, hand: Hand) {
        for (i in 0..50) {
            world.addParticle(
                DustParticleEffect(Vec3f(0f, 0f, 0f), 1f),
                user.x + world.random.nextGaussian() * user.width/2.0,
                user.randomBodyY,
                user.z + world.random.nextGaussian() * user.width/2.0,
                0.0,
                0.5,
                0.0
            )
        }

        if (user is ServerPlayerEntity && world is ServerWorld) {
            for (i in 0..9) {
                world.spawnEntity(BatEntity(EntityType.BAT, world).also {
                    it.setPos(
                        user.x + world.random.nextGaussian() * user.width/2.0,
                        user.randomBodyY,
                        user.z + world.random.nextGaussian() * user.width/2.0
                    )
                    it.setVelocity(0.0, 0.15, 0.0)
                    it.playSound(SoundEvents.ENTITY_BAT_TAKEOFF, 1.0f, 0.8f+world.random.nextGaussian().toFloat()*0.3f)
                })
            }

            val spawnWorld = world.registryManager[Registry.WORLD_KEY][user.spawnPointDimension]
            if (spawnWorld !is ServerWorld) {
                return
            }

            val spawnPos = user.spawnPointPosition?.let { pos ->
                PlayerEntity.findRespawnPosition(spawnWorld, pos, user.spawnAngle, false, true)
                    .map { Pair(it, user.spawnAngle) }
                    .get()
            } ?: Pair(Vec3d.ofBottomCenter(spawnWorld.spawnPos), spawnWorld.spawnAngle)

            user.teleport(spawnWorld, spawnPos.first.x, spawnPos.first.y, spawnPos.first.z, spawnPos.second, 0.0f)
        }
    }
}