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

class InvisibilityAbilityEntityComponent(val player: LivingEntity): InvisibilityAbilityComponent, AutoSyncedComponent {
    private val syncCallback = { _: KProperty<*>, _: Any?, _: Any? ->
        if (!player.world.isClient) {
            InvisibilityAbilityComponent.entityKey.sync(player)
        }
    }

    override var invisTicks: Long by Delegates.observable(0, syncCallback)

    override fun serverTick() {
        val invisLevel = (player).getAbilityLevel(AbilityModule.INVISIBILITY)
        if (player.vampireComponent.blood >= 16 && invisLevel > 0 && player.isSneaking && player.world.time-invisTicks >= 120 + invisLevel*60) {
            if (player is ServerPlayerEntity) {
                UseInvisibilityCriterion.trigger(player)
            }
            invisTicks = player.world.time
            player.addStatusEffect(StatusEffectInstance(StatusEffects.INVISIBILITY, invisLevel*player.world.gameRules[HaemaGameRules.invisLength].get(), 0))
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
