package com.williambl.haema.ability.component.dash

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.criteria.UseDashCriterion
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.isVampire
import com.williambl.haema.util.HaemaGameRules
import com.williambl.haema.util.raytraceForDash
import com.williambl.haema.vampireComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.mob.PathAwareEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.nbt.NbtCompound
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.util.math.Vec3d
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

class AiControlledEntityDashAbilityComponent(entity: LivingEntity, val targetGetter: () -> Vec3d?): EntityDashAbilityComponent(entity) {
    override fun dash() {
        if (!this.canDash()) {
            return
        }

        val world = this.entity.world
        val target = this.targetGetter() ?: return

        if (world is ServerWorld) {
            val rand = world.random
            for (j in 0 until 3) {
                val x: Double = (target.x - entity.x) * rand.nextDouble() + entity.x - 0.5
                val y: Double = (target.y - entity.y) * rand.nextDouble() + entity.y + 1
                val z: Double = (target.z - entity.z) * rand.nextDouble() + entity.z - 0.5
                world.spawnParticles(
                    DustParticleEffect.DEFAULT,
                    x, y, z,
                    10,
                    0.5, 1.0, 0.5,
                    0.0
                )
            }

            world.playSound(
                null,
                target.x,
                target.y,
                target.z,
                SoundEvents.ENTITY_GHAST_SHOOT,
                SoundCategory.PLAYERS,
                1f,
                1.5f
            )

            this.entity.teleport(target.x, target.y, target.z)
            if (this.entity is ServerPlayerEntity) {
                UseDashCriterion.trigger(this.entity)
            }
        }

        this.lastDashed = world.time
    }
}
