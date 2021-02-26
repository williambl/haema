package com.williambl.haema.util

import com.mojang.brigadier.StringReader
import com.mojang.brigadier.arguments.ArgumentType
import com.mojang.brigadier.context.CommandContext
import com.mojang.brigadier.exceptions.CommandSyntaxException
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType
import com.mojang.brigadier.suggestion.Suggestions
import com.mojang.brigadier.suggestion.SuggestionsBuilder
import com.williambl.haema.VampireAbility
import net.minecraft.command.CommandSource
import net.minecraft.server.command.ServerCommandSource
import net.minecraft.text.LiteralText
import java.util.concurrent.CompletableFuture

class VampireAbilityArgumentType private constructor() : ArgumentType<VampireAbility> {
    @Throws(CommandSyntaxException::class)
    override fun parse(stringReader: StringReader): VampireAbility {
        val string = stringReader.readUnquotedString()
        try {
        return VampireAbility.valueOf(string.toUpperCase())
        } catch (e: IllegalArgumentException) {
            throw INVALID_ABILITY_EXCEPTION.create(string)
        }
    }

    override fun <S> listSuggestions(
        context: CommandContext<S>,
        builder: SuggestionsBuilder
    ): CompletableFuture<Suggestions> {
        return CommandSource.suggestMatching(VampireAbility.values().map(VampireAbility::name), builder)
    }

    override fun getExamples(): Collection<String> {
        return EXAMPLES
    }

    companion object {
        private val EXAMPLES: Collection<String> = listOf("STRENGTH", "INVISIBILITY")
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
}