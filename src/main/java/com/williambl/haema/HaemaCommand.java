package com.williambl.haema;

import com.mojang.authlib.GameProfile;
import com.mojang.brigadier.Command;
import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.DoubleArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.SuggestionProvider;
import com.williambl.haema.api.content.blood.BloodApi;
import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.VampirismSource;
import com.williambl.haema.api.vampire.ability.VampireAbilitiesComponent;
import com.williambl.haema.api.vampire.ability.VampireAbility;
import com.williambl.haema.hunters.VampireHunterContractItem;
import com.williambl.haema.hunters.VampireHunterSpawner;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.GameProfileArgument;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

import java.util.*;
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
    public static final String NO_SUCH_ABILITY_KEY = "command.haema.error.no_such_ability";
    private static final DynamicCommandExceptionType NO_SUCH_ABILITY = new DynamicCommandExceptionType((source) -> Component.translatable(NO_SUCH_ABILITY_KEY, source));

    private static final SuggestionProvider<CommandSourceStack> SUGGEST_VAMPIRISM_SOURCES = suggestFromRegistry(VampirismSource.REGISTRY_KEY);
    private static final SuggestionProvider<CommandSourceStack> SUGGEST_ABILITIES = suggestFromRegistry(VampireAbility.REGISTRY_KEY);

    private static <T> SuggestionProvider<CommandSourceStack> suggestFromRegistry(ResourceKey<Registry<T>> registryKey) {
        return (ctx, builder) -> SharedSuggestionProvider.suggestResource(ctx.getSource().registryAccess().registryOrThrow(registryKey).keySet(), builder);
    }

    //todo permissions
    public static void register(CommandDispatcher<CommandSourceStack> dispatcher, CommandBuildContext registryAccess, Commands.CommandSelection environment) {
        dispatcher.register(
                literal("haema").then(
                        literal("convert").then(
                                argument("targets", EntityArgument.entities()).executes(HaemaCommand::convert).then(
                                        argument("source", ResourceLocationArgument.id()).suggests(SUGGEST_VAMPIRISM_SOURCES).executes(HaemaCommand::convertWithArgSource)))).then(
                        literal("deconvert").then(
                                argument("targets", EntityArgument.entities()).executes(HaemaCommand::deconvert).then(
                                        argument("source", ResourceLocationArgument.id()).suggests(SUGGEST_VAMPIRISM_SOURCES).executes(HaemaCommand::deconvertWithArgSource)))).then(
                        literal("query").then(
                                argument("targets", EntityArgument.entities()).executes(HaemaCommand::query))).then(
                        bloodTree()).then(
                        abilitiesTree()).then(
                        literal("create_multiblock_pattern").then(
                                argument("corner1", BlockPosArgument.blockPos()).then(
                                        argument("corner2", BlockPosArgument.blockPos()).executes(HaemaCommand::createMultiblockPattern)))).then(
                        hunterTree())
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
                        argument("amount", DoubleArgumentType.doubleArg()).executes(HaemaCommand::removeBlood)))).then(
                literal("quality").then(
                        argument("targets", EntityArgument.entities()).executes(HaemaCommand::queryBloodQuality))
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
                        argument("ability", ResourceLocationArgument.id()).suggests(SUGGEST_ABILITIES).then(
                        argument("value", BoolArgumentType.bool()).executes(HaemaCommand::setAbility))))
        );
        //@formatter:on
    }

    private static ArgumentBuilder<CommandSourceStack, ?> hunterTree() {
        //@formatter:off
        return literal("hunter").then(
                literal("spawn_patrol").then(
                        argument("position", BlockPosArgument.blockPos()).executes(HaemaCommand::spawnPatrol))).then(
                literal("create_targeted_contract").executes(HaemaCommand::createContractWithRandomTarget).then(
                        argument("target", EntityArgument.entity()).executes(HaemaCommand::createContractWithTarget)));
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
                ctx.getSource().sendSuccess(() -> Component.translatable(CONVERT_SUCCESS, entity.getDisplayName()), true);
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
                ctx.getSource().sendSuccess(() -> Component.translatable(DECONVERT_SUCCESS, entity.getDisplayName()), true);
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

            ctx.getSource().sendSuccess(() -> Component.translatable(QUERY_FIRST_LINE, entity.getDisplayName()).withStyle(ChatFormatting.WHITE), false);
            ctx.getSource().sendSuccess(() -> feedback(QUERY_IS_VAMPIRE_LINE, entity.getDisplayName(), isVampireText, vampirismSourceText), false);
            ctx.getSource().sendSuccess(() -> bloodQueryMessage(entity, component), false);
            for (var text : abilityQueryMessage(entity, abilityRegistry)) {
                ctx.getSource().sendSuccess(() -> text, false);
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
            ctx.getSource().sendSuccess(() -> bloodQueryMessage(entity, component), false);
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
            ctx.getSource().sendSuccess(() -> feedback(BLOOD_SET_SUCCESS, entity.getDisplayName(), blood(component.getBlood())), true);
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
            ctx.getSource().sendSuccess(() -> feedback(BLOOD_SET_SUCCESS, entity.getDisplayName(), blood(component.getBlood())), true);
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
            ctx.getSource().sendSuccess(() -> feedback(BLOOD_SET_SUCCESS, entity.getDisplayName(), blood(component.getBlood())), true);
        }

        return entities.size();
    }

    private static int queryBloodQuality(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        var entities = EntityArgument.getEntities(ctx, "targets");

        for (var entity : entities) {
            var quality = BloodApi.getBloodQuality(entity);
            ctx.getSource().sendSuccess(() -> bloodQualityQueryMessage(entity, quality), false);
        }

        return entities.size();
    }

    public static final String BLOOD_QUALITY_QUERY_RESULT = "command.haema.blood.quality.query.result";

    public static final String BLOOD_QUALITY_QUERY_NONE = "command.haema.blood.quality.query.none";
    private static Component bloodQualityQueryMessage(Entity entity, Optional<BloodQuality> quality) {
        return quality.map(q -> feedback(BLOOD_QUALITY_QUERY_RESULT, entity.getDisplayName(), q.text())).orElseGet(() ->
                feedback(BLOOD_QUALITY_QUERY_NONE, entity.getDisplayName()));
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
                ctx.getSource().sendSuccess(() -> text, false);
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
        if (ability == null) {
            throw NO_SUCH_ABILITY.create(abilityId);
        }

        for (var entity : entities) {
            var component = VampireAbilitiesComponent.KEY.getNullable(entity);
            if (component == null) {
                throw CANNOT_HAVE_ABILITIES.create(entity.getDisplayName());
            }

            if (enabled) {
                component.addAbility(ability);
                ctx.getSource().sendSuccess(() -> feedback(ABILITY_ADDED, entity.getDisplayName(), ability(abilityId)), true);
            } else {
                component.removeAbility(ability);
                ctx.getSource().sendSuccess(() -> feedback(ABILITY_REMOVED, entity.getDisplayName(), ability(abilityId)), true);
            }
        }

        return entities.size();
    }

    public static final String CREATE_MULTIBLOCK_PATTERN_TOO_MANY_BLOCKSTATES = "command.haema.multiblock.create_pattern.too_many_blockstates";

    private static int createMultiblockPattern(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        BlockPos corner1 = BlockPosArgument.getLoadedBlockPos(ctx, "corner1");
        BlockPos corner2 = BlockPosArgument.getLoadedBlockPos(ctx, "corner2");

        int minX = Math.min(corner1.getX(), corner2.getX());
        int minY = Math.min(corner1.getY(), corner2.getY());
        int minZ = Math.min(corner1.getZ(), corner2.getZ());
        int maxX = Math.max(corner1.getX(), corner2.getX());
        int maxY = Math.max(corner1.getY(), corner2.getY());
        int maxZ = Math.max(corner1.getZ(), corner2.getZ());

        int sizeX = maxX - minX + 1;
        int sizeY = maxY - minY + 1;
        int sizeZ = maxZ - minZ + 1;

        // pattern is stored as [y][z][x] b/c it makes more sense when displayed as string arrays
        char[][][] pattern = new char[sizeY][sizeZ][sizeX];
        Map<BlockState, Character> stateCharacters = new HashMap<>();
        stateCharacters.put(Blocks.STRUCTURE_VOID.defaultBlockState(), ' ');
        BlockPos.MutableBlockPos pos = new BlockPos.MutableBlockPos(minX, minY, minZ);
        for (int y = 0; y < sizeY; y++) {
            pos.setY(minY + y);
            for (int z = 0; z < sizeZ; z++) {
                pos.setZ(minZ + z);
                for (int x = 0; x < sizeX; x++) {
                    pos.setX(minX + x);
                    BlockState state = ctx.getSource().getLevel().getBlockState(pos);
                    Character c = stateCharacters.get(state);
                    if (c == null) {
                        c = getNonMappedChar(BuiltInRegistries.BLOCK.getKey(state.getBlock()).getPath(), stateCharacters, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!£$%^&*@".toCharArray());
                        if (c == '?') {
                            ctx.getSource().sendSuccess(() -> warning(CREATE_MULTIBLOCK_PATTERN_TOO_MANY_BLOCKSTATES), false);
                        }
                        stateCharacters.put(state, c);
                    }
                    pattern[y][z][x] = c;
                }
            }
        }

        StringBuilder patternBuilder = new StringBuilder();
        for (int y = 0; y < sizeY; y++) {
            for (int z = 0; z < sizeZ; z++) {
                patternBuilder.append('"');
                for (int x = 0; x < sizeX; x++) {
                    patternBuilder.append(pattern[y][z][x]);
                }
                patternBuilder.append('"');
                patternBuilder.append('\n');
            }
            patternBuilder.append('\n');
        }

        for (var entry : stateCharacters.entrySet()) {
            patternBuilder.append("%s = %s%n".formatted(entry.getValue(), entry.getKey()));
        }

        ctx.getSource().sendSuccess(() -> Component.literal(patternBuilder.toString()), false);
        Haema.LOGGER.info(patternBuilder.toString());

        return Command.SINGLE_SUCCESS;
    }

    private static char getNonMappedChar(String name, Map<BlockState, Character> stateCharacters, char[] extras) {
        char[] candidates = new char[extras.length + 2];
        candidates[0] = name.charAt(0);
        candidates[1] = Character.toUpperCase(candidates[0]);
        System.arraycopy(extras, 0, candidates, 2, extras.length);
        for (char c : candidates) {
            if (!stateCharacters.containsValue(c)) {
                return c;
            }
        }

        return '?';
    }


    public static final String SPAWN_PATROL_SUCCESS = "command.haema.hunter.spawn_patrol.success";
    public static final String SPAWN_PATROL_FAILURE = "command.haema.hunter.spawn_patrol.failure";

    private static int spawnPatrol(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        BlockPos pos = BlockPosArgument.getLoadedBlockPos(ctx, "position");
        int amountSpawned = VampireHunterSpawner.trySpawnNear(ctx.getSource().getLevel(), ctx.getSource().getLevel().getRandom(), pos);
        if (amountSpawned > 0) {
            ctx.getSource().sendSuccess(() -> feedback(SPAWN_PATROL_SUCCESS, amountSpawned, pos), true);
            return amountSpawned;
        } else {
            ctx.getSource().sendFailure(Component.translatable(SPAWN_PATROL_FAILURE, pos));
            return 0;
        }
    }

    public static final String CREATE_CONTRACT_SUCCESS = "command.haema.hunter.create_contract.success";
    public static final String CREATE_CONTRACT_NO_TARGET = "command.haema.hunter.create_contract.no_target";

    private static int createContract(CommandContext<CommandSourceStack> ctx, @Nullable Player target) throws CommandSyntaxException {
        var stack = target == null
                ? VampireHunterContractItem.createWithRandomTarget(ctx.getSource().getLevel())
                : VampireHunterContractItem.createWithTarget(target);
        ctx.getSource().getPlayerOrException().addItem(stack);
        var actualTarget = VampireHunterContractItem.getContractTarget(stack);
        ctx.getSource().sendSuccess(() -> actualTarget.map(p -> feedback(CREATE_CONTRACT_SUCCESS, p.getName())).orElseGet(() -> warning(CREATE_CONTRACT_NO_TARGET)), false);
        return actualTarget.isPresent() ? Command.SINGLE_SUCCESS : 2;
    }

    private static int createContractWithRandomTarget(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return createContract(ctx, null);
    }

    private static int createContractWithTarget(CommandContext<CommandSourceStack> ctx) throws CommandSyntaxException {
        return createContract(ctx, EntityArgument.getPlayer(ctx, "target"));
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

    private static MutableComponent warning(String key, Object... params) {
        return Component.translatable(key, params).withStyle(ChatFormatting.YELLOW);
    }
}
