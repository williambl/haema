package com.williambl.haema;

import com.mojang.brigadier.Command;
import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.core.Registry;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

import static com.williambl.haema.Haema.id;
import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public final class HaemaCommand {
    private static final DynamicCommandExceptionType NO_SUCH_VAMPIRISM_SOURCE = new DynamicCommandExceptionType((source) -> Component.translatable("command.haema.error.no_such_vampirism_source", source));
    private static final DynamicCommandExceptionType NOT_VAMPIRABLE = new DynamicCommandExceptionType((source) -> Component.translatable("command.haema.error.not_vampirable", source));

    //todo permissions
    public static void register(CommandDispatcher<CommandSourceStack> dispatcher, CommandBuildContext registryAccess, Commands.CommandSelection environment) {
        dispatcher.register(
                literal("haema").then(
                        literal("convert").then(
                                argument("targets", EntityArgument.entities()).executes(HaemaCommand::convert).then(
                                        argument("source", ResourceLocationArgument.id()).executes(HaemaCommand::convertWithArgSource)))).then(
                        literal("deconvert").then(
                                argument("targets", EntityArgument.entities()).executes(HaemaCommand::deconvert).then(
                                        argument("source", ResourceLocationArgument.id()).executes(HaemaCommand::deconvertWithArgSource)))).then(
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

    private static int convert(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return convert(ctx, () -> id("command"));
    }

    private static int convertWithArgSource(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return convert(ctx, () -> ResourceLocationArgument.getId(ctx, "source"));
    }

    private static int convert(CommandContext<CommandSourceStack> ctx, Supplier<ResourceLocation> sourceSupplier) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");
        ResourceLocation sourceKey = sourceSupplier.get();
        Registry<VampirismSource> sourceRegistry = ctx.getSource().registryAccess().registryOrThrow(VampirismSource.REGISTRY_KEY);
        if (!sourceRegistry.containsKey(sourceKey)) {
            throw NO_SUCH_VAMPIRISM_SOURCE.create(sourceKey);
        }
        VampirismSource source = sourceRegistry.get(sourceKey);

        int i = 0;
        for (var entity : entities) {
            var component = VampireComponent.KEY.getNullable(entity);
            if (component == null) {
                throw NOT_VAMPIRABLE.create(entity.getDisplayName());
            }

            if (component.tryConvert(source)) {
                ctx.getSource().sendSuccess(Component.translatable("command.haema.convert.success", entity.getDisplayName()), true);
                i++;
            } else {
                ctx.getSource().sendFailure(Component.translatable("command.haema.convert.failure", entity.getDisplayName()));
            }
        }

        return i;
    }

    private static int deconvert(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return convert(ctx, () -> id("command"));
    }

    private static int deconvertWithArgSource(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        int source = convert(ctx, () -> ResourceLocationArgument.getId(ctx, "source"));
        return source;
    }

    private static int deconvert(CommandContext<CommandSourceStack> ctx, Supplier<ResourceLocation> sourceSupplier) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");
        ResourceLocation sourceKey = sourceSupplier.get();
        Registry<VampirismSource> sourceRegistry = ctx.getSource().registryAccess().registryOrThrow(VampirismSource.REGISTRY_KEY);
        if (!sourceRegistry.containsKey(sourceKey)) {
            throw NO_SUCH_VAMPIRISM_SOURCE.create(sourceKey);
        }
        VampirismSource source = sourceRegistry.get(sourceKey);

        int i = 0;
        for (var entity : entities) {
            var component = VampireComponent.KEY.getNullable(entity);
            if (component == null) {
                throw NOT_VAMPIRABLE.create(entity.getDisplayName());
            }

            if (component.tryCure(source)) {
                ctx.getSource().sendSuccess(Component.translatable("command.haema.deconvert.success", entity.getDisplayName()), true);
                i++;
            } else {
                ctx.getSource().sendFailure(Component.translatable("command.haema.deconvert.failure", entity.getDisplayName()));
            }
        }

        return i;
    }

    private static int query(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");
        for (var entity : entities) {
            var component = VampireComponent.KEY.getNullable(entity);
            if (component == null) {
                throw NOT_VAMPIRABLE.create(entity.getDisplayName());
            }

            Registry<VampirismSource> sourceRegistry = ctx.getSource().registryAccess().registryOrThrow(VampirismSource.REGISTRY_KEY);
            Registry<VampireAbility> abilityRegistry = ctx.getSource().registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY);

            Component isVampireText = component.isVampire()
                    ? Component.translatable("command.haema.query.yes").withStyle(ChatFormatting.GREEN)
                    : Component.translatable("command.haema.query.no").withStyle(ChatFormatting.RED);
            VampirismSource source = component.getVampirismSource();
            Component vampirismSourceText = source == null
                    ? Component.empty()
                    : Component.translatable("command.haema.query.vampirism_source", Component.literal(Objects.requireNonNull(sourceRegistry.getKey(source)).toString()).withStyle(ChatFormatting.GOLD)).withStyle(ChatFormatting.GRAY);
            Set<VampireAbility> abilities = VampireAbilitiesComponent.KEY.maybeGet(entity).map(VampireAbilitiesComponent::getAbilities).orElseGet(Set::of);
            Component abilityCountText = (abilities.isEmpty() ? Component.translatable("command.haema.query.no_abilities_line", entity.getDisplayName()) : Component.translatable("command.haema.query.abilities_line", entity.getDisplayName(), abilities)).withStyle(ChatFormatting.GRAY);

            ctx.getSource().sendSuccess(Component.translatable("command.haema.query.first_line", entity.getDisplayName()).withStyle(ChatFormatting.WHITE), false);
            ctx.getSource().sendSuccess(Component.translatable("command.haema.query.is_vampire_line", entity.getDisplayName(), isVampireText, vampirismSourceText).withStyle(ChatFormatting.GRAY), false);
            ctx.getSource().sendSuccess(Component.translatable("command.haema.query.blood_line", entity.getDisplayName(), Component.literal(String.valueOf(component.getBlood())).withStyle(ChatFormatting.DARK_RED)).withStyle(ChatFormatting.GRAY), false);
            ctx.getSource().sendSuccess(abilityCountText, false);
            for (var ability : abilities) {
                ctx.getSource().sendSuccess(Component.translatable("command.haema.query.ability_line", Component.literal(Objects.requireNonNull(abilityRegistry.getKey(ability)).toString()).withStyle(ChatFormatting.GOLD)).withStyle(ChatFormatting.GRAY), false);
            }
        }

        return entities.size();
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
