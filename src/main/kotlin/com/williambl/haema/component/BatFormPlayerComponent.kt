package com.williambl.haema.component

import com.williambl.haema.abilities.VampireAbility
import com.williambl.haema.abilities.abilityRegistry
import dev.onyxstudios.cca.api.v3.component.CopyableComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import nerdhub.cardinal.components.api.ComponentType
import net.minecraft.entity.Entity
import net.minecraft.nbt.CompoundTag
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.Identifier
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

@Suppress("UnstableApiUsage")
class BatFormPlayerComponent(player: Entity) : BatFormComponent, AutoSyncedComponent, CopyableComponent<BatFormPlayerComponent> {
    private val syncCallback = { _: KProperty<*>, _: Any?, _: Any? ->
        if (!player.world.isClient) {
            BatFormComponent.entityKey.sync(player)
        }
    }

    override var isBat: Boolean by Delegates.observable(false, syncCallback)

    override fun writeToNbt(tag: CompoundTag) {}
    override fun readFromNbt(tag: CompoundTag) {}

    override fun copyFrom(other: BatFormPlayerComponent) {}

    override fun writeSyncPacket(buf: PacketByteBuf, recipient: ServerPlayerEntity) {
        buf.writeBoolean(isBat)
    }

    override fun applySyncPacket(buf: PacketByteBuf) {
        isBat = buf.readBoolean()
    }

    override fun getComponentType(): ComponentType<*> {
        throw UnsupportedOperationException()
    }
}