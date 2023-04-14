package com.williambl.haema;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.ResourceLocationArgument;

import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public final class HaemaCommand {
    //todo permissions
    public static void register(CommandDispatcher<CommandSourceStack> dispatcher, CommandBuildContext registryAccess, Commands.CommandSelection environment) {
        dispatcher.register(
                literal("haema").then(
                        literal("convert").then(
                                argument("targets", EntityArgument.entities()).executes(HaemaCommand::convert).then(
                                        argument("source", ResourceLocationArgument.id()).executes(HaemaCommand::convert)))).then(
                        literal("deconvert").then(
                                argument("targets", EntityArgument.entities()).executes(HaemaCommand::deconvert).then(
                                        argument("source", ResourceLocationArgument.id()).executes(HaemaCommand::convert)))).then(
                        literal("query").then(
                                argument("targets", EntityArgument.entities()).executes(HaemaCommand::query))).then(
                        bloodTree()).then(
                        abilitiesTree())
        );
    }

    private static ArgumentBuilder<CommandSourceStack, ?> bloodTree() {
        //@formatter:off
        return literal("blood").then(
                literal("get").then(
                        argument("targets", EntityArgument.entities()).executes(HaemaCommand::getBlood))).then(
                literal("set").then(
                        argument("targets", EntityArgument.entities()).then(
                        argument("amount", IntegerArgumentType.integer()).executes(HaemaCommand::setBlood)))).then(
                literal("add").then(
                        argument("targets", EntityArgument.entities()).then(
                        argument("amount", IntegerArgumentType.integer()).executes(HaemaCommand::addBlood)))).then(
                literal("remove").then(
                        argument("targets", EntityArgument.entities()).then(
                        argument("amount", IntegerArgumentType.integer()).executes(HaemaCommand::removeBlood)))
        );
        //@formatter:on
    }

    private static ArgumentBuilder<CommandSourceStack, ?> abilitiesTree() {
        //@formatter:off
        return literal("abilities").then(
                literal("get").then(
                        argument("targets", EntityArgument.entities()).executes(HaemaCommand::getAbilities))).then(
                literal("set").then(
                        argument("targets", EntityArgument.entities()).then(
                        argument("ability", ResourceLocationArgument.id()).then(
                        argument("value", BoolArgumentType.bool()).executes(HaemaCommand::setAbility))))
        );
        //@formatter:on
    }

    private static int convert(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }

    private static int deconvert(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }

    private static int query(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }

    private static int getBlood(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }

    private static int setBlood(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }

    private static int addBlood(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }

    private static int removeBlood(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }

    private static int getAbilities(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }

    private static int setAbility(CommandContext<CommandSourceStack> ctx) {
        //TODO
        return Command.SINGLE_SUCCESS;
    }
}
