package com.williambl.haema.criteria

import com.google.gson.JsonObject
import com.williambl.haema.id
import net.minecraft.advancement.criterion.AbstractCriterion
import net.minecraft.advancement.criterion.AbstractCriterionConditions
import net.minecraft.predicate.entity.AdvancementEntityPredicateDeserializer
import net.minecraft.predicate.entity.EntityPredicate
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.Identifier

object UseDashCriterion: AbstractCriterion<UseDashCriterion.Conditions>() {
    private val id = id("use_dash")
    override fun getId(): Identifier = id

    override fun conditionsFromJson(
        obj: JsonObject,
        playerPredicate: EntityPredicate.Extended,
        predicateDeserializer: AdvancementEntityPredicateDeserializer
    ): Conditions = Conditions(playerPredicate)

    fun trigger(player: ServerPlayerEntity) {
        this.trigger(player) { true }
    }

    class Conditions(playerPredicate: EntityPredicate.Extended): AbstractCriterionConditions(id, playerPredicate)
}