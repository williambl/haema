package com.williambl.haema.criteria

import com.google.gson.JsonObject
import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.VampireAbility
import com.williambl.haema.id
import net.minecraft.advancement.criterion.AbstractCriterion
import net.minecraft.advancement.criterion.AbstractCriterionConditions
import net.minecraft.predicate.entity.AdvancementEntityPredicateDeserializer
import net.minecraft.predicate.entity.EntityPredicate
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.Identifier

object AbilityChangeCriterion: AbstractCriterion<AbilityChangeCriterion.Conditions>() {
    private val id = id("ability_level_up")
    override fun getId(): Identifier = id

    override fun conditionsFromJson(
        obj: JsonObject,
        playerPredicate: EntityPredicate.Extended,
        predicateDeserializer: AdvancementEntityPredicateDeserializer
    ): Conditions = Conditions(playerPredicate, (obj["ability"] as String?)?.let { Identifier(it) }, (obj["level"] as Int?))

    fun trigger(player: ServerPlayerEntity, ability: VampireAbility, level: Int) {
        this.trigger(player) { it.matches(ability, level) }
    }

    class Conditions(playerPredicate: EntityPredicate.Extended, private val abilityId: Identifier?, private val level: Int?):
        AbstractCriterionConditions(id, playerPredicate) {
            fun matches(ability: VampireAbility, abilityLevel: Int): Boolean {
                var result = true
                if (abilityId != null)
                    result = result and (AbilityModule.ABILITY_REGISTRY.getId(ability) == abilityId)
                if (level != null)
                    result = result and (abilityLevel == level)

                return result
            }
    }
}