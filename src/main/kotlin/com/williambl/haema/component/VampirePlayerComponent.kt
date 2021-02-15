package com.williambl.haema.component

import dev.onyxstudios.cca.api.v3.component.CopyableComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import nerdhub.cardinal.components.api.ComponentType
import net.minecraft.entity.Entity
import net.minecraft.nbt.CompoundTag
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

@Suppress("UnstableApiUsage")
class VampirePlayerComponent(player: Entity) : VampireComponent, AutoSyncedComponent, CopyableComponent<VampirePlayerComponent> {
    private val syncCallback = { _: KProperty<*>, _: Any?, _: Any? ->
        if (!player.world.isClient) {
            VampireComponent.entityKey.sync(player)
        }
    }

    override var isVampire: Boolean by Delegates.observable(false, syncCallback)
    override var isPermanentVampire: Boolean by Delegates.observable(false, syncCallback)
    override var isKilled: Boolean by Delegates.observable(false, syncCallback)

    override fun writeToNbt(tag: CompoundTag) {
        tag.putBoolean("isVampire", isVampire)
        tag.putBoolean("isPermanentVampire", isPermanentVampire)
        tag.putBoolean("isKilled", isKilled)
    }

    override fun readFromNbt(tag: CompoundTag) {
        isVampire = tag.getBoolean("isVampire")
        isPermanentVampire = tag.getBoolean("isPermanentVampire")
        isKilled = tag.getBoolean("isKilled")
    }

    override fun copyFrom(other: VampirePlayerComponent) {
        isVampire = other.isVampire
        isPermanentVampire = other.isPermanentVampire
    }

    override fun getComponentType(): ComponentType<*> {
        throw UnsupportedOperationException()
    }

}