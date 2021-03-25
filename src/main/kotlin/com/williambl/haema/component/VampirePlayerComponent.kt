package com.williambl.haema.component

import com.williambl.haema.abilities.VampireAbility
import com.williambl.haema.abilities.abilityRegistry
import dev.onyxstudios.cca.api.v3.component.CopyableComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import nerdhub.cardinal.components.api.ComponentType
import net.minecraft.entity.Entity
import net.minecraft.nbt.CompoundTag
import net.minecraft.util.Identifier
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

    override var abilities: MutableMap<VampireAbility, Int> = mutableMapOf(
        VampireAbility.STRENGTH to 1,
        VampireAbility.DASH to 1,
        VampireAbility.INVISIBILITY to 0,
        VampireAbility.IMMORTALITY to 1,
        VampireAbility.VISION to 1
    )

    override var ritualsUsed: MutableSet<Identifier> = mutableSetOf()

    override fun writeToNbt(tag: CompoundTag) {
        tag.putBoolean("isVampire", isVampire)
        tag.putBoolean("isPermanentVampire", isPermanentVampire)
        tag.putBoolean("isKilled", isKilled)
        tag.put("abilities", CompoundTag().also { abilitiesTag -> abilities.forEach { (ability, value) -> abilitiesTag.putInt(abilityRegistry.getId(ability).toString(), value) } })
        tag.put("ritualsUsed", CompoundTag().also {
            it.putInt("Length", ritualsUsed.size)
            ritualsUsed.forEachIndexed { idx, id -> it.putString(idx.toString(), id.toString()) }
        })
    }

    override fun readFromNbt(tag: CompoundTag) {
        isVampire = tag.getBoolean("isVampire")
        isPermanentVampire = tag.getBoolean("isPermanentVampire")
        isKilled = tag.getBoolean("isKilled")
        val abilitiesTag = tag.getCompound("abilities")
        abilityRegistry.entries.filter { abilitiesTag.contains(it.key.value.toString()) }.forEach { abilities[it.value] = abilitiesTag.getInt(it.key.value.toString()) }
        val ritualsUsedTag = tag.getCompound("ritualsUsed")
        ritualsUsed = List(ritualsUsedTag.getInt("Length")) { idx -> Identifier(ritualsUsedTag.getString(idx.toString())) }.toMutableSet()
    }

    override fun copyFrom(other: VampirePlayerComponent) {
        isVampire = other.isVampire
        isPermanentVampire = other.isPermanentVampire
        abilities = other.abilities
        ritualsUsed = other.ritualsUsed
    }

    override fun getComponentType(): ComponentType<*> {
        throw UnsupportedOperationException()
    }

}