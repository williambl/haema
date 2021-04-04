package com.williambl.haema.criteria

import com.williambl.haema.api.DrinkBloodEvent
import com.williambl.haema.api.VampireConversionEvents
import net.fabricmc.fabric.api.`object`.builder.v1.advancement.CriterionRegistry
import net.minecraft.server.network.ServerPlayerEntity

fun registerCriteria() {
    CriterionRegistry.register(VampireConversionCriterion)
    VampireConversionEvents.CONVERT.register { player ->
        if (player is ServerPlayerEntity)
            VampireConversionCriterion.trigger(player)
    }

    CriterionRegistry.register(VampireDeconversionCriterion)
    VampireConversionEvents.DECONVERT.register { player ->
        if (player is ServerPlayerEntity)
            VampireDeconversionCriterion.trigger(player)
    }

    CriterionRegistry.register(DrinkBloodCriterion)
    DrinkBloodEvent.EVENT.register { drinker, target, _ ->
        if (drinker is ServerPlayerEntity)
            DrinkBloodCriterion.trigger(drinker, target)
    }

}