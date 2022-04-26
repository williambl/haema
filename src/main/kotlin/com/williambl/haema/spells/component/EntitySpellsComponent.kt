package com.williambl.haema.spells.component

import com.williambl.haema.Haema
import com.williambl.haema.spells.SpellInstance
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.nbt.NbtCompound
import net.minecraft.nbt.NbtElement
import net.minecraft.nbt.NbtList
import net.minecraft.nbt.NbtOps
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity

class EntitySpellsComponent(val entity: LivingEntity): SpellsComponent, AutoSyncedComponent {
    override var spells: MutableList<SpellInstance> = mutableListOf()

    override fun addSpell(instance: SpellInstance) {
        spells.add(instance)
        this.sync()
    }

    override fun removeSpell(instance: SpellInstance) {
        spells.remove(instance)
        this.sync()
    }

    override fun clearSpells() {
        spells.clear()
        this.sync()
    }

    override fun readFromNbt(tag: NbtCompound) {
        spells = if (tag.contains("spells", NbtElement.LIST_TYPE.toInt())) {
            SpellInstance.CODEC.listOf().decode(NbtOps.INSTANCE, tag.get("spells")).getOrThrow(true, Haema.LOGGER::error).first.toMutableList()
        } else {
            mutableListOf()
        }

        this.sync()
    }

    override fun writeToNbt(tag: NbtCompound) {
        tag.put("spells", SpellInstance.CODEC.listOf().encode(spells, NbtOps.INSTANCE, NbtList()).getOrThrow(true, Haema.LOGGER::error))
    }

    override fun writeSyncPacket(buf: PacketByteBuf, recipient: ServerPlayerEntity) {
        buf.writeVarInt(spells.size)
        spells.forEach { spell -> spell.toBytes(buf) }
    }

    override fun applySyncPacket(buf: PacketByteBuf) {
        spells.clear()
        val amount = buf.readVarInt()
        for (i in 0 until amount) {
            spells.add(SpellInstance(buf))
        }
    }

    override fun tick() {
        val world = entity.world

        spells.retainAll { spell ->
            if (spell.timeSpellCharged <= world.time) {
                spell.spell.use(world, entity)
                false
            } else {
                spell.spell.createChargeParticles(world, entity)
                true
            }
        }

        this.sync()
    }

    fun sync() {
        SpellsComponent.entityKey.sync(entity)
    }
}