package com.williambl.haema;

import com.williambl.dfunc.DFunction;
import com.williambl.dfunc.DFunctionType;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.powers.sunlight_sickness.VampireBurnEvents;
import net.minecraft.core.Registry;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;

import java.util.function.Supplier;

import static com.williambl.haema.Haema.id;

public final class HaemaDFunctions {
    public static final DFunctionType<Entity, Double, ? extends Supplier<? extends DFunction<Entity, Double>>> BLOOD = Registry.register(
            DFunction.ENTITY_TO_NUMBER_FUNCTION_TYPE_REGISTRY.registry(),
            id("blood"),
            DFunction.<Entity, Double>create(e -> VampireComponent.KEY.maybeGet(e).map(VampireComponent::getBlood).orElse(0.0)));

    public static final DFunctionType<Entity, Double, ? extends Supplier<? extends DFunction<Entity, Double>>> MAX_BLOOD = Registry.register(
            DFunction.ENTITY_TO_NUMBER_FUNCTION_TYPE_REGISTRY.registry(),
            id("max_blood"),
            DFunction.<Entity, Double>create(e -> VampireComponent.MAX_BLOOD));

    public static final DFunctionType<Entity, Boolean, ? extends Supplier<? extends DFunction<Entity, Boolean>>> TRIGGER_BURN_EVENT = Registry.register(
            DFunction.ENTITY_PREDICATE_TYPE_REGISTRY.registry(),
            id("trigger_burn_event"),
            DFunction.<Entity, Boolean>create(e -> e instanceof LivingEntity l && VampireBurnEvents.TRIGGER.invoker().shouldTriggerVampireBurn(l)));

    public static final DFunctionType<Entity, Boolean, ? extends Supplier<? extends DFunction<Entity, Boolean>>> PREVENT_BURN_EVENT = Registry.register(
            DFunction.ENTITY_PREDICATE_TYPE_REGISTRY.registry(),
            id("prevent_burn_event"),
            DFunction.<Entity, Boolean>create(e -> e instanceof LivingEntity l && VampireBurnEvents.PREVENT.invoker().shouldPreventVampireBurn(l)));


    static void init() {}
}
