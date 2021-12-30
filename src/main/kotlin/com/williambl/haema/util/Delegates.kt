package com.williambl.haema.util

import dev.onyxstudios.cca.api.v3.component.sync.ComponentPacketWriter
import net.minecraft.network.PacketByteBuf
import kotlin.properties.ObservableProperty
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KMutableProperty
import kotlin.reflect.KProperty


/**
 * A property synced from server->client.
 *
 * Properties using the `synced` delegate are automatically synced when changed on the server. They act transparently,
 * looking just like normal properties; however, do not change them too often (i.e. multiple times a tick), as every
 * change sends a packet to all clients!
 *
 * @param initialValue the initial value of the property.
 * @param ownerIdProp the property holding the UUID of the [GameObject][com.williambl.raycastengine.gameobject.GameObject] which owns this property.
 * @param toBytes a function used to serialise the property, used when syncing.
 * @param fromBytes a function used to deserialise the property, used when syncing.
 */
fun <T> synced(
    initialValue: T,
    syncCallback: (ComponentPacketWriter) -> (property: KProperty<*>, oldValue: T, newValue: T) -> Unit,
    toBytes: (PacketByteBuf, T) -> Unit,
    fromBytes: (PacketByteBuf) -> T
): ReadWriteProperty<Any?, T> =
    SyncedProperty(initialValue, syncCallback, toBytes, fromBytes)

class SyncedProperty<T>(
    initialValue: T,
    private val syncCallback: (ComponentPacketWriter) -> (property: KProperty<*>, oldValue: T, newValue: T) -> Unit,
    private val toBytes: (PacketByteBuf, T) -> Unit,
    private val fromBytes: (PacketByteBuf) -> T
) : ObservableProperty<T>(initialValue) {
    override fun afterChange(property: KProperty<*>, oldValue: T, newValue: T) {
        syncCallback { packetByteBuf, _ ->
            packetByteBuf.writeVarInt(1)
            writeToBytes(property, newValue, packetByteBuf)
        }(property, oldValue, newValue)
    }

    fun writeToBytes(property: KProperty<*>, value: T, buf: PacketByteBuf) {
        buf.writeString(property.name)
        toBytes(buf, value)
    }

    fun setFromBytes(property: KMutableProperty<*>, receiver: Any?, buf: PacketByteBuf) {
        property.setter.call(receiver, fromBytes(buf))
    }
}

