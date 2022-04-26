package com.williambl.haema.spells

import com.mojang.serialization.Codec
import com.mojang.serialization.codecs.RecordCodecBuilder
import net.minecraft.network.PacketByteBuf

data class SpellInstance(val spell: Spell, val timeSpellCharged: Long) {
    constructor(packetByteBuf: PacketByteBuf) : this(SpellsModule.SPELL_REGISTRY.get(packetByteBuf.readVarInt())!!, packetByteBuf.readVarLong())

    fun toBytes(packetByteBuf: PacketByteBuf): PacketByteBuf {
        packetByteBuf.writeVarInt(SpellsModule.SPELL_REGISTRY.getRawId(spell))
        packetByteBuf.writeVarLong(timeSpellCharged)
        return packetByteBuf
    }

    companion object {
        val CODEC: Codec<SpellInstance> = RecordCodecBuilder.create { instance ->
            instance.group(
                SpellsModule.SPELL_REGISTRY.codec.fieldOf("spell").forGetter(SpellInstance::spell),
                Codec.LONG.fieldOf("timeSpellCharged").forGetter(SpellInstance::timeSpellCharged)
            ).apply(instance, ::SpellInstance)
        }
    }
}
