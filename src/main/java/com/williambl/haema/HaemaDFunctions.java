package com.williambl.haema;

import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.ContextArg;
import com.williambl.dfunc.api.type.DFunctionType;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.powers.sunlight_sickness.VampireBurnEvents;
import net.minecraft.core.Registry;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

import java.util.function.Function;

import static com.williambl.haema.Haema.id;

public final class HaemaDFunctions {
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
