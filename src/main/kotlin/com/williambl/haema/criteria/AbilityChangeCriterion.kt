package com.williambl.haema.criteria

import com.google.gson.JsonObject
import com.williambl.haema.abilities.VampireAbility
import net.minecraft.advancement.criterion.AbstractCriterion
import net.minecraft.advancement.criterion.AbstractCriterionConditions
import net.minecraft.predicate.entity.AdvancementEntityPredicateDeserializer
import net.minecraft.predicate.entity.EntityPredicate
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.Identifier

object AbilityChangeCriterion: AbstractCriterion<AbilityChangeCriterion.Conditions>() {
    private val id = Identifier("haema:ability_level_up")
    override fun getId(): Identifier = id

    override fun conditionsFromJson(
        obj: JsonObject,
        playerPredicate: EntityPredicate.Extended,
        predicateDeserializer: AdvancementEntityPredicateDeserializer
    ): Conditions = Conditions(playerPredicate, (obj["ability"] as String?)?.let { Identifier(it) }, (obj["level"] as Int?))

    fun trigger(player: ServerPlayerEntity, ability: VampireAbility, level: Int) {
        this.test(player) { it.matches(ability, level) }
    }

    class Conditions(playerPredicate: EntityPredicate.Extended, private val abilityId: Identifier?, private val level: Int?):
        AbstractCriterionConditions(id, playerPredicate) {
            fun matches(ability: VampireAbility, abilityLevel: Int): Boolean {
                var result = true
                if (abilityId != null)
                    result = result and TODO("ability registry")
                if (level != null)
                    result = result and (abilityLevel == level)

                return result
            }
    }
}