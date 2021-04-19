package com.williambl.haema.abilities

import com.google.gson.JsonObject
import com.mojang.brigadier.StringReader
import com.mojang.brigadier.arguments.ArgumentType
import com.mojang.brigadier.context.CommandContext
import com.mojang.brigadier.exceptions.CommandSyntaxException
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType
import com.mojang.brigadier.suggestion.Suggestions
import com.mojang.brigadier.suggestion.SuggestionsBuilder
import net.minecraft.command.CommandSource
import net.minecraft.command.argument.serialize.ArgumentSerializer
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.command.ServerCommandSource
import net.minecraft.text.LiteralText
import net.minecraft.util.Identifier
import java.util.concurrent.CompletableFuture

class VampireAbilityArgumentType private constructor() : ArgumentType<VampireAbility> {
    @Throws(CommandSyntaxException::class)
    override fun parse(stringReader: StringReader): VampireAbility {
        val id = Identifier.fromCommandInput(stringReader)
        return abilityRegistry.getOrEmpty(id).orElseThrow {
            INVALID_ABILITY_EXCEPTION.create(id)
        }
    }

    override fun <S> listSuggestions(
        context: CommandContext<S>,
        builder: SuggestionsBuilder
    ): CompletableFuture<Suggestions> {
        return CommandSource.suggestMatching(abilityRegistry.ids.map(Identifier::toString), builder)
    }

    override fun getExamples(): Collection<String> {
        return EXAMPLES
    }

    companion object {
        private val EXAMPLES: Collection<String> = listOf("haema:strength", "haema:invisibility")
        val INVALID_ABILITY_EXCEPTION = DynamicCommandExceptionType { LiteralText("Invalid ability: $it") }

        fun ability(): VampireAbilityArgumentType {
            return VampireAbilityArgumentType()
        }

        fun getAbility(context: CommandContext<ServerCommandSource?>, name: String?): VampireAbility {
            return context.getArgument(
                name,
                VampireAbility::class.java
            ) as VampireAbility
        }
    }

    object Serialiser: ArgumentSerializer<VampireAbilityArgumentType> {
        override fun toPacket(argumentType: VampireAbilityArgumentType, packetByteBuf: PacketByteBuf) = Unit

        override fun fromPacket(packetByteBuf: PacketByteBuf): VampireAbilityArgumentType = VampireAbilityArgumentType()

        override fun toJson(argumentType: VampireAbilityArgumentType, jsonObject: JsonObject) = Unit
    }
}