package com.williambl.haema;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.DoubleArgumentType;
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
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;

import static com.williambl.haema.Haema.id;
import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public final class HaemaCommand {
    public static final String NO_SUCH_VAMPIRISM_SOURCE_KEY = "command.haema.error.no_such_vampirism_source";
    private static final DynamicCommandExceptionType NO_SUCH_VAMPIRISM_SOURCE = new DynamicCommandExceptionType((source) -> Component.translatable(NO_SUCH_VAMPIRISM_SOURCE_KEY, source));
    public static final String NOT_VAMPIRABLE_KEY = "command.haema.error.not_vampirable";
    private static final DynamicCommandExceptionType NOT_VAMPIRABLE = new DynamicCommandExceptionType((source) -> Component.translatable(NOT_VAMPIRABLE_KEY, source));
    public static final String CANNOT_HAVE_ABILITIES_KEY = "command.haema.error.cannot_have_abilities";
    private static final DynamicCommandExceptionType CANNOT_HAVE_ABILITIES = new DynamicCommandExceptionType((source) -> Component.translatable(CANNOT_HAVE_ABILITIES_KEY, source));

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
                literal("query").then(
                        argument("targets", EntityArgument.entities()).executes(HaemaCommand::queryBlood))).then(
                literal("set").then(
                        argument("targets", EntityArgument.entities()).then(
                        argument("amount", DoubleArgumentType.doubleArg()).executes(HaemaCommand::setBlood)))).then(
                literal("add").then(
                        argument("targets", EntityArgument.entities()).then(
                        argument("amount", DoubleArgumentType.doubleArg()).executes(HaemaCommand::addBlood)))).then(
                literal("remove").then(
                        argument("targets", EntityArgument.entities()).then(
                        argument("amount", DoubleArgumentType.doubleArg()).executes(HaemaCommand::removeBlood)))
        );
        //@formatter:on
    }

    private static ArgumentBuilder<CommandSourceStack, ?> abilitiesTree() {
        //@formatter:off
        return literal("abilities").then(
                literal("query").then(
                        argument("targets", EntityArgument.entities()).executes(HaemaCommand::queryAbilities))).then(
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

    public static final String CONVERT_SUCCESS = "command.haema.convert.success";
    public static final String CONVERT_FAILURE = "command.haema.convert.failure";

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
                ctx.getSource().sendSuccess(Component.translatable(CONVERT_SUCCESS, entity.getDisplayName()), true);
                i++;
            } else {
                ctx.getSource().sendFailure(Component.translatable(CONVERT_FAILURE, entity.getDisplayName()));
            }
        }

        return i;
    }

    private static int deconvert(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return deconvert(ctx, () -> id("command"));
    }

    private static int deconvertWithArgSource(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        int source = deconvert(ctx, () -> ResourceLocationArgument.getId(ctx, "source"));
        return source;
    }

    public static final String DECONVERT_SUCCESS = "command.haema.deconvert.success";
    public static final String DECONVERT_FAILURE = "command.haema.deconvert.failure";

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
                ctx.getSource().sendSuccess(Component.translatable(DECONVERT_SUCCESS, entity.getDisplayName()), true);
                i++;
            } else {
                ctx.getSource().sendFailure(Component.translatable(DECONVERT_FAILURE, entity.getDisplayName()));
            }
        }

        return i;
    }

    public static final String QUERY_FIRST_LINE = "command.haema.query.first_line";
    public static final String QUERY_IS_VAMPIRE_LINE = "command.haema.query.is_vampire_line";
    public static final String QUERY_BLOOD_LINE = "command.haema.query.blood_line";
    public static final String QUERY_YES = "command.haema.query.yes";
    public static final String QUERY_NO = "command.haema.query.no";
    public static final String QUERY_VAMPIRISM_SOURCE = "command.haema.query.vampirism_source";
    public static final String QUERY_NO_ABILITIES_LINE = "command.haema.query.no_abilities_line";
    public static final String QUERY_ABILITIES_LINE = "command.haema.query.abilities_line";
    public static final String QUERY_ABILITY_LINE = "command.haema.query.ability_line";

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
                    ? Component.translatable(QUERY_YES).withStyle(ChatFormatting.GREEN)
                    : Component.translatable(QUERY_NO).withStyle(ChatFormatting.RED);
            VampirismSource source = component.getVampirismSource();
            Component vampirismSourceText = source == null
                    ? Component.empty()
                    : feedback(QUERY_VAMPIRISM_SOURCE, ability(sourceRegistry.getKey(source)));

            ctx.getSource().sendSuccess(Component.translatable(QUERY_FIRST_LINE, entity.getDisplayName()).withStyle(ChatFormatting.WHITE), false);
            ctx.getSource().sendSuccess(feedback(QUERY_IS_VAMPIRE_LINE, entity.getDisplayName(), isVampireText, vampirismSourceText), false);
            ctx.getSource().sendSuccess(bloodQueryMessage(entity, component), false);
            for (var text : abilityQueryMessage(entity, abilityRegistry)) {
                ctx.getSource().sendSuccess(text, false);
            }
        }

        return entities.size();
    }

    private static Component bloodQueryMessage(Entity entity, VampireComponent component) {
        return feedback(QUERY_BLOOD_LINE, entity.getDisplayName(), blood(component.getBlood()));
    }

    private static List<Component> abilityQueryMessage(Entity entity, Registry<VampireAbility> abilityRegistry) {
        List<Component> components = new ArrayList<>();
        Set<VampireAbility> abilities = VampireAbilitiesComponent.KEY.maybeGet(entity).map(VampireAbilitiesComponent::getAbilities).orElseGet(Set::of);
        Component abilityCountText = (abilities.isEmpty() ? feedback(QUERY_NO_ABILITIES_LINE, entity.getDisplayName())
                : feedback(QUERY_ABILITIES_LINE, entity.getDisplayName(), abilities.size()));
        components.add(abilityCountText);
        for (var ability : abilities) {
            components.add(feedback(QUERY_ABILITY_LINE, ability(abilityRegistry.getKey(ability))));
        }

        return components;
    }


    private static int queryBlood(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");

        for (var entity : entities) {
            var component = VampireComponent.KEY.getNullable(entity);
            if (component == null) {
                throw NOT_VAMPIRABLE.create(entity.getDisplayName());
            }
            ctx.getSource().sendSuccess(bloodQueryMessage(entity, component), false);
        }

        return entities.size();
    }

    public static final String BLOOD_SET_SUCCESS = "command.haema.blood.set.success";

    private static int setBlood(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");
        double amount = DoubleArgumentType.getDouble(ctx, "amount");

        for (var entity : entities) {
            var component = VampireComponent.KEY.getNullable(entity);
            if (component == null) {
                throw NOT_VAMPIRABLE.create(entity.getDisplayName());
            }

            component.setBlood(amount);
            ctx.getSource().sendSuccess(feedback(BLOOD_SET_SUCCESS, entity.getDisplayName(), blood(component.getBlood())), true);
        }

        return entities.size();
    }


    private static int addBlood(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");
        double amount = DoubleArgumentType.getDouble(ctx, "amount");

        for (var entity : entities) {
            var component = VampireComponent.KEY.getNullable(entity);
            if (component == null) {
                throw NOT_VAMPIRABLE.create(entity.getDisplayName());
            }

            component.addBlood(amount);
            ctx.getSource().sendSuccess(feedback(BLOOD_SET_SUCCESS, entity.getDisplayName(), blood(component.getBlood())), true);
        }

        return entities.size();
    }


    private static int removeBlood(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");
        double amount = DoubleArgumentType.getDouble(ctx, "amount");

        for (var entity : entities) {
            var component = VampireComponent.KEY.getNullable(entity);
            if (component == null) {
                throw NOT_VAMPIRABLE.create(entity.getDisplayName());
            }

            component.removeBlood(amount);
            ctx.getSource().sendSuccess(feedback(BLOOD_SET_SUCCESS, entity.getDisplayName(), blood(component.getBlood())), true);
        }

        return entities.size();
    }

    private static int queryAbilities(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");
        var registry = ctx.getSource().registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY);

        for (var entity : entities) {
            var component = VampireComponent.KEY.getNullable(entity);
            if (component == null) {
                throw NOT_VAMPIRABLE.create(entity.getDisplayName());
            }
            for (var text : abilityQueryMessage(entity, registry)) {
                ctx.getSource().sendSuccess(text, false);
            }
        }

        return entities.size();
    }

    public static final String ABILITY_ADDED = "command.haema.ability.set.added";
    public static final String ABILITY_REMOVED = "command.haema.ability.set.removed";

    private static int setAbility(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");
        var abilityId = ResourceLocationArgument.getId(ctx, "ability");
        boolean enabled = BoolArgumentType.getBool(ctx, "value");
        var registry = ctx.getSource().registryAccess().registryOrThrow(VampireAbility.REGISTRY_KEY);
        var ability = registry.get(abilityId);

        for (var entity : entities) {
            var component = VampireAbilitiesComponent.KEY.getNullable(entity);
            if (component == null) {
                throw CANNOT_HAVE_ABILITIES.create(entity.getDisplayName());
            }

            if (enabled) {
                component.addAbility(ability);
                ctx.getSource().sendSuccess(feedback(ABILITY_ADDED, entity.getDisplayName(), ability(abilityId)), true);
            } else {
                component.removeAbility(ability);
                ctx.getSource().sendSuccess(feedback(ABILITY_REMOVED, entity.getDisplayName(), ability(abilityId)), true);
            }
        }

        return entities.size();
    }

    private static MutableComponent feedback(String key, Object... params) {
        return Component.translatable(key, params).withStyle(ChatFormatting.GRAY);
    }

    private static MutableComponent ability(@Nullable ResourceLocation key) {
        return Component.literal(key == null ? "unknown" : key.toString()).withStyle(ChatFormatting.GOLD);
    }

    private static MutableComponent blood(double amount) {
        return Component.literal("%.2f".formatted(amount)).withStyle(ChatFormatting.DARK_RED);
    }
}
