package com.williambl.haema;

import com.williambl.dfunc.api.DFunctions;
import com.williambl.dfunc.api.DTypes;
import com.williambl.dfunc.api.functions.EntityDFunctions;
import com.williambl.haema.api.vampire.VampireComponent;
import com.williambl.haema.api.vampire.ability.powers.sunlight_sickness.VampireBurnEvents;
import com.williambl.vampilang.lang.EvaluationContext;
import com.williambl.vampilang.lang.VValue;
import com.williambl.vampilang.lang.function.VFunctionDefinition;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;

import java.util.Map;

import static com.williambl.haema.Haema.id;

public final class HaemaDFunctions {
    public static final EvaluationContext.Spec ENTITY_DAMAGE_WITH_WEAPON = new EvaluationContext.Spec(Map.of("entity", DTypes.ENTITY, "level", DTypes.LEVEL, "damage_source", DTypes.DAMAGE_SOURCE, "damage_amount", StandardVTypes.NUMBER, "attacker", DTypes.ENTITY, "direct_attacker", DTypes.ENTITY, "weapon", DTypes.ITEM_STACK));

    public static EvaluationContext entityDamageWithWeapon(Entity entity, DamageSource source, float amount, ItemStack weapon) {
        return EvaluationContext.builder(ENTITY_DAMAGE_WITH_WEAPON)
                .addVariable("entity", new VValue(DTypes.ENTITY, entity))
                .addVariable("level", new VValue(DTypes.LEVEL, entity.level()))
                .addVariable("damage_source", new VValue(DTypes.DAMAGE_SOURCE, source))
                .addVariable("damage_amount", new VValue(StandardVTypes.NUMBER, (double) amount))
                .addVariable("attacker", new VValue(DTypes.ENTITY, source.getEntity()))
                .addVariable("direct_attacker", new VValue(DTypes.ENTITY, source.getDirectEntity()))
                .addVariable("weapon", new VValue(DTypes.ITEM_STACK, weapon))
                .build();
    }

    public static final VFunctionDefinition BLOOD = EntityDFunctions.createSimpleNumber(id("blood").toString(), e -> VampireComponent.KEY.maybeGet(e).map(VampireComponent::getBlood).orElse(0.0));
    public static final VFunctionDefinition MAX_BLOOD = EntityDFunctions.createSimpleNumber(id("max_blood").toString(), e -> VampireComponent.MAX_BLOOD);

    public static final VFunctionDefinition TRIGGER_BURN_EVENT = EntityDFunctions.createFromPredicate(id("trigger_burn_event").toString(),
                    (e) -> e instanceof LivingEntity l && VampireBurnEvents.TRIGGER.invoker().shouldTriggerVampireBurn(l));

    public static final VFunctionDefinition PREVENT_BURN_EVENT = EntityDFunctions.createFromPredicate(id("prevent_burn_event").toString(),
            (e) -> e instanceof LivingEntity l && VampireBurnEvents.PREVENT.invoker().shouldPreventVampireBurn(l));

    static void init() {
        DFunctions.ENV.registerFunction(BLOOD);
        DFunctions.ENV.registerFunction(MAX_BLOOD);
        DFunctions.ENV.registerFunction(TRIGGER_BURN_EVENT);
        DFunctions.ENV.registerFunction(PREVENT_BURN_EVENT);
    }
}
