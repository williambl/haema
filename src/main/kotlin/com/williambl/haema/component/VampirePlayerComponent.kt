package com.williambl.haema.component

import dev.onyxstudios.cca.api.v3.component.CopyableComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import nerdhub.cardinal.components.api.ComponentType
import net.minecraft.nbt.CompoundTag

@Suppress("UnstableApiUsage")
class VampirePlayerComponent : VampireComponent, AutoSyncedComponent, CopyableComponent<VampirePlayerComponent> {
    override var isVampire: Boolean = false
    override var isPermanentVampire: Boolean = false
    override var isKilled: Boolean = false

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