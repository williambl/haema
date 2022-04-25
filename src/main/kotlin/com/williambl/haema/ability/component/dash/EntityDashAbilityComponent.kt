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
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.nbt.NbtCompound
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

class EntityDashAbilityComponent(val entity: LivingEntity): DashAbilityComponent, AutoSyncedComponent {
    private val syncCallback = { _: KProperty<*>, _: Any?, _: Any? ->
        if (!entity.world.isClient) {
            DashAbilityComponent.entityKey.sync(entity)
        }
    }

    override var lastDashed: Long by Delegates.observable(0, syncCallback)

    var dashCooldownValue: Int by Delegates.observable(entity.world.gameRules.getInt(HaemaGameRules.dashCooldown), syncCallback)

    override fun canDash(): Boolean =
        (entity.isVampire)
                && (entity.getAbilityLevel(AbilityModule.DASH) > 0)
                && (entity.vampireComponent.blood >= 18 || entity is PlayerEntity && entity.isCreative)
                && (this.entity.world.time >= this.lastDashed + (dashCooldownValue.toLong() * (1 + AbilityModule.DASH.maxLevel - entity.getAbilityLevel(AbilityModule.DASH))))

    override fun dash() {
        if (!canDash()) {
            return
        }

        val world = entity.world
        val target = raytraceForDash(entity)

        if (target != null && world is ServerWorld) {
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

            entity.teleport(target.x, target.y, target.z)
            if (entity is ServerPlayerEntity) {
                UseDashCriterion.trigger(entity)
            }
        }

        lastDashed = world.time
    }

    override fun updateDashCooldown(newValue: Int) {
        this.dashCooldownValue = newValue
    }

    override fun writeSyncPacket(buf: PacketByteBuf, recipient: ServerPlayerEntity) {
        buf.writeVarLong(lastDashed)
        buf.writeVarInt(dashCooldownValue)
    }

    override fun applySyncPacket(buf: PacketByteBuf) {
        lastDashed = buf.readVarLong()
        dashCooldownValue = buf.readVarInt()
    }

    override fun writeToNbt(tag: NbtCompound) {
    }

    override fun readFromNbt(tag: NbtCompound) {
    }
}
