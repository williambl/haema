package com.williambl.haema.criteria

import com.williambl.haema.api.AbilityChangeEvent
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.api.VampireConversionEvents
import net.fabricmc.api.ModInitializer
import net.minecraft.advancement.criterion.Criteria
import net.minecraft.server.network.ServerPlayerEntity

object CriteriaModule: ModInitializer {
    override fun onInitialize() {
        Criteria.register(VampireConversionCriterion)
        VampireConversionEvents.CONVERT.register { player ->
            if (player is ServerPlayerEntity)
                VampireConversionCriterion.trigger(player)
        }

        Criteria.register(VampireDeconversionCriterion)
        VampireConversionEvents.DECONVERT.register { player ->
            if (player is ServerPlayerEntity)
                VampireDeconversionCriterion.trigger(player)
        }

        Criteria.register(DrinkBloodCriterion)
        BloodDrinkingEvents.ON_BLOOD_DRINK.register { drinker, target, _ ->
            if (drinker is ServerPlayerEntity)
                DrinkBloodCriterion.trigger(drinker, target)
        }

        Criteria.register(AbilityChangeCriterion)
        AbilityChangeEvent.EVENT.register { vampire, ability, level ->
            if (vampire is ServerPlayerEntity)
                AbilityChangeCriterion.trigger(vampire, ability, level)
        }

        Criteria.register(UseDashCriterion)
        Criteria.register(UseInvisibilityCriterion)
        Criteria.register(VampireConversionFailureCriterion)
        Criteria.register(StoreBloodCriterion)
        Criteria.register(VampireHunterTriggerCriterion)
        Criteria.register(UseRitualCriterion)
        Criteria.register(UseMistCriterion)
    }
}