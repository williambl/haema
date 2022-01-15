package com.williambl.haema.ability.component.mist_form

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.component.invisibility.InvisibilityAbilityComponent
import com.williambl.haema.criteria.UseInvisibilityCriterion
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.isVampire
import com.williambl.haema.util.HaemaGameRules
import com.williambl.haema.vampireComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.nbt.NbtCompound
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.ParticleTypes
import net.minecraft.server.network.ServerPlayerEntity
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

class EntityMistFormAbilityComponent(val entity: LivingEntity): MistFormAbilityComponent {
    private val syncCallback = { _: KProperty<*>, _: Any?, _: Any? ->
        if (!entity.world.isClient) {
            MistFormAbilityComponent.entityKey.sync(entity)
        }
    }

    override var isInMistForm: Boolean by Delegates.observable(false, syncCallback)

    override fun serverTick() {
    }

    override fun clientTick() {
        if (isInMistForm) {
            val rand = entity.random
            val dims = entity.getDimensions(entity.pose)
            for (i in 0..30) {
                entity.world.addParticle(
                    ParticleTypes.DRIPPING_WATER,
                    entity.x - (dims.width/2.0) + rand.nextGaussian() * dims.width,
                    entity.randomBodyY,
                    entity.z - (dims.width/2.0) + rand.nextGaussian() * dims.width,
                    rand.nextGaussian() * 0.3,
                    rand.nextGaussian() * 0.3,
                    rand.nextGaussian() * 0.3
                )
            }
        }
    }

    override fun writeSyncPacket(buf: PacketByteBuf, recipient: ServerPlayerEntity) {
        buf.writeBoolean(isInMistForm)
    }

    override fun applySyncPacket(buf: PacketByteBuf) {
        isInMistForm = buf.readBoolean()
    }

    override fun writeToNbt(tag: NbtCompound) {
        tag.putBoolean("isInMistForm", isInMistForm)
    }

    override fun readFromNbt(tag: NbtCompound) {
        isInMistForm = tag.getBoolean("isInMistForm")
    }
}
