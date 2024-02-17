package com.williambl.haema.ritual.ritual;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunctions;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.ritual.RitualTrigger;
import com.williambl.vampilang.lang.VExpression;
import com.williambl.vampilang.stdlib.StandardVTypes;
import net.minecraft.util.KeyDispatchDataCodec;

import java.util.function.Function;

public record RightClickRitualTrigger(VExpression predicate) implements RitualTrigger {
    public static final KeyDispatchDataCodec<RightClickRitualTrigger> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunctions.resolvedExpressionCodec(StandardVTypes.BOOLEAN, DFunctions.ENTITY_INTERACT_WITH_BLOCK).fieldOf("predicate").forGetter(RightClickRitualTrigger::predicate)
    ).apply(instance, RightClickRitualTrigger::new)));

    @Override
    public KeyDispatchDataCodec<? extends RitualTrigger> codec() {
        return CODEC;
    }
}
