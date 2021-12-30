package com.williambl.haema.ability.component

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.criteria.UseInvisibilityCriterion
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.util.HaemaGameRules
import com.williambl.haema.vampireComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.nbt.NbtCompound
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

class EntityInvisibilityAbilityComponent(val entity: LivingEntity): InvisibilityAbilityComponent, AutoSyncedComponent {
    private val syncCallback = { _: KProperty<*>, _: Any?, _: Any? ->
        if (!entity.world.isClient) {
            InvisibilityAbilityComponent.entityKey.sync(entity)
        }
    }

    override var invisTicks: Long by Delegates.observable(0, syncCallback)

    override fun serverTick() {
        val invisLevel = (entity).getAbilityLevel(AbilityModule.INVISIBILITY)
        if (entity.vampireComponent.blood >= 16 && invisLevel > 0 && entity.isSneaking && entity.world.time-invisTicks >= 120 + invisLevel*60) {
            if (entity is ServerPlayerEntity) {
                UseInvisibilityCriterion.trigger(entity)
            }
            invisTicks = entity.world.time
            entity.addStatusEffect(StatusEffectInstance(StatusEffects.INVISIBILITY, invisLevel*entity.world.gameRules[HaemaGameRules.invisLength].get(), 0))
        }
    }

    override fun writeSyncPacket(buf: PacketByteBuf, recipient: ServerPlayerEntity) {
        buf.writeVarLong(invisTicks)
    }

    override fun applySyncPacket(buf: PacketByteBuf) {
        invisTicks = buf.readVarLong()
    }

    override fun writeToNbt(tag: NbtCompound) {
    }

    override fun readFromNbt(tag: NbtCompound) {
    }
}
