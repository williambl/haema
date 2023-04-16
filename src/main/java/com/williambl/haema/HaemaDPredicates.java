package com.williambl.haema;

import com.mojang.serialization.Codec;
import com.williambl.dpred.DPredicate;
import com.williambl.dpred.DPredicateType;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.powers.sunlight_sickness.VampireBurnEvents;
import net.minecraft.core.Registry;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;

import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

import static com.williambl.haema.Haema.id;

public final class HaemaDPredicates {
    public static final DPredicateType<Entity, ? extends Function<DPredicate<Double>, ? extends DPredicate<Entity>>> BLOOD = Registry.register(
            DPredicate.ENTITY_PREDICATE_TYPE_REGISTRY.registry(),
            id("blood"),
            DPredicate.<DPredicate<Double>, Entity>create(
                    DPredicate.NUMBER_PREDICATE_TYPE_REGISTRY.codec().fieldOf("predicate"),
                    (predicate, e) -> VampireComponent.KEY.maybeGet(e).map(VampireComponent::getBlood).map(predicate::test).orElse(false)));

    public static final DPredicateType<Entity, ? extends BiFunction<DPredicate<Double>, Boolean, ? extends DPredicate<Entity>>> HEALTH_RELATIVE_TO_MAX = Registry.register(
            DPredicate.ENTITY_PREDICATE_TYPE_REGISTRY.registry(),
            id("health_relative_to_max"),
            DPredicate.<DPredicate<Double>, Boolean, Entity>create(
                    DPredicate.NUMBER_PREDICATE_TYPE_REGISTRY.codec().fieldOf("predicate"),
                    Codec.BOOL.optionalFieldOf("include_max_modifiers", true),
                    (predicate, includeModifiers, e) -> e instanceof LivingEntity l
                            && predicate.test(l.getHealth() - (includeModifiers ? l.getAttributeValue(Attributes.MAX_HEALTH) : l.getAttributeBaseValue(Attributes.MAX_HEALTH)))));

    public static final DPredicateType<Entity, ? extends Supplier<? extends DPredicate<Entity>>> TRIGGER_BURN_EVENT = Registry.register(
            DPredicate.ENTITY_PREDICATE_TYPE_REGISTRY.registry(),
            id("trigger_burn_event"),
            DPredicate.<Entity>create(e -> e instanceof LivingEntity l && VampireBurnEvents.TRIGGER.invoker().shouldTriggerVampireBurn(l)));

    public static final DPredicateType<Entity, ? extends Supplier<? extends DPredicate<Entity>>> PREVENT_BURN_EVENT = Registry.register(
            DPredicate.ENTITY_PREDICATE_TYPE_REGISTRY.registry(),
            id("prevent_burn_event"),
            DPredicate.<Entity>create(e -> e instanceof LivingEntity l && VampireBurnEvents.PREVENT.invoker().shouldPreventVampireBurn(l)));


    static void init() {}
}
