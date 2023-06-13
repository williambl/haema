package com.williambl.haema;

import com.google.common.reflect.TypeToken;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.ContextArg;
import com.williambl.dfunc.api.context.DFContext;
import com.williambl.dfunc.api.context.DFContextSpec;
import com.williambl.dfunc.api.type.DFunctionType;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.powers.sunlight_sickness.VampireBurnEvents;
import net.minecraft.core.Registry;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.pattern.BlockInWorld;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

import static com.williambl.haema.Haema.id;

public final class HaemaDFunctions {
    public static final DFContextSpec ENTITY_DAMAGE_WITH_WEAPON = new DFContextSpec(Map.of("entity", TypeToken.of(Entity.class), "level", TypeToken.of(Level.class), "damage_source", TypeToken.of(DamageSource.class), "damage_amount", TypeToken.of(Double.class), "attacker", new TypeToken<Optional<Entity>>() {}, "direct_attacker", new TypeToken<Optional<Entity>>() {}, "weapon", TypeToken.of(ItemStack.class)));
    public static final DFContextSpec BLOCK_IN_WORLD = new DFContextSpec(Map.of("block", TypeToken.of(BlockInWorld.class), "level", TypeToken.of(Level.class)));

    public static DFContext entityDamageWithWeapon(Entity entity, DamageSource source, float amount, ItemStack weapon) {
        return DFContext.builder()
                .addArgument("entity", entity)
                .addArgument("level", entity.getLevel())
                .addArgument("damage_source", source)
                .addArgument("damage_amount", (double) amount)
                .addArgument("attacker", Optional.ofNullable(source.getEntity()), new TypeToken<>() {})
                .addArgument("direct_attacker", Optional.ofNullable(source.getDirectEntity()), new TypeToken<>() {})
                .addArgument("weapon", weapon)
                .build();
    }

    public static DFContext blockInWorld(BlockInWorld block) {
        return DFContext.builder()
                .addArgument("block", block)
                .addArgument("level", block.getLevel())
                .build();
    }

    private static final TypeToken<BlockInWorld> BLOCK_TYPE = TypeToken.of(BlockInWorld.class);
    private static final TypeToken<Level> LEVEL_TYPE = TypeToken.of(Level.class);

    public static DFContext blockInWorldFast(BlockInWorld block) {
        return new DFContext(Map.of("block", block, "level", block.getLevel()), Map.of(BLOCK_TYPE, List.of(block), LEVEL_TYPE, List.of(block.getLevel())));
    }

    public static final DFunctionType<Double, ? extends Function<ContextArg<Entity>, ? extends DFunction<Double>>> BLOOD = Registry.register(
            DFunction.NUMBER_FUNCTION.registry(),
            id("blood"),
            DFunction.<ContextArg<Entity>, Double>create(
                    ContextArg.ENTITY,
                    (e, ctx) -> VampireComponent.KEY.maybeGet(e.get(ctx)).map(VampireComponent::getBlood).orElse(0.0)));

    public static final DFunctionType<Double, ? extends Function<ContextArg<Entity>, ? extends DFunction<Double>>> MAX_BLOOD = Registry.register(
            DFunction.NUMBER_FUNCTION.registry(),
            id("max_blood"),
            DFunction.<ContextArg<Entity>, Double>create(
                    ContextArg.ENTITY,
                    (e, ctx) -> VampireComponent.MAX_BLOOD));

    public static final DFunctionType<Boolean, ? extends Function<ContextArg<Entity>, ? extends DFunction<Boolean>>> TRIGGER_BURN_EVENT = Registry.register(
            DFunction.PREDICATE.registry(),
            id("trigger_burn_event"),
            DFunction.<ContextArg<Entity>, Boolean>create(
                    ContextArg.ENTITY,
                    (e, ctx) -> e.get(ctx) instanceof LivingEntity l && VampireBurnEvents.TRIGGER.invoker().shouldTriggerVampireBurn(l)));

    public static final DFunctionType<Boolean, ? extends Function<ContextArg<Entity>, ? extends DFunction<Boolean>>> PREVENT_BURN_EVENT = Registry.register(
            DFunction.PREDICATE.registry(),
            id("prevent_burn_event"),
            DFunction.<ContextArg<Entity>, Boolean>create(
                    ContextArg.ENTITY,
                    (e, ctx) -> e.get(ctx) instanceof LivingEntity l && VampireBurnEvents.PREVENT.invoker().shouldPreventVampireBurn(l)));

    static void init() {}
}
