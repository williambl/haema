package com.williambl.haema.ritual.ritual;

import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.haema.HaemaDFunctions;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.ritual.RitualTrigger;
import net.minecraft.util.KeyDispatchDataCodec;

import java.util.function.Function;

public record RightClickRitualTrigger(DFunction<Boolean> predicate) implements RitualTrigger {
    private static final KeyDispatchDataCodec<RightClickRitualTrigger> CODEC = KeyDispatchDataCodec.of(RecordCodecBuilder.create(instance -> instance.group(
            DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(HaemaDFunctions.ENTITY_INTERACT_WITH_BLOCK), Function.identity()).fieldOf("predicate").forGetter(RightClickRitualTrigger::predicate)
    ).apply(instance, RightClickRitualTrigger::new)));

    @Override
    public KeyDispatchDataCodec<? extends RitualTrigger> codec() {
        return CODEC;
    }
}
