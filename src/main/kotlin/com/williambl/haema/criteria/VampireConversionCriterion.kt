package com.williambl.haema.criteria

import com.google.gson.JsonObject
import com.williambl.haema.id
import net.minecraft.advancement.criterion.AbstractCriterion
import net.minecraft.advancement.criterion.AbstractCriterionConditions
import net.minecraft.predicate.entity.AdvancementEntityPredicateDeserializer
import net.minecraft.predicate.entity.LootContextPredicate
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.Identifier

object VampireConversionCriterion: AbstractCriterion<VampireConversionCriterion.Conditions>() {
    private val id = id("conversion")
    override fun getId(): Identifier = id

    override fun conditionsFromJson(
        obj: JsonObject,
        playerPredicate: LootContextPredicate,
        predicateDeserializer: AdvancementEntityPredicateDeserializer
    ): Conditions = Conditions(playerPredicate)

    fun trigger(player: ServerPlayerEntity) {
        this.trigger(player) { true }
    }

    class Conditions(playerPredicate: LootContextPredicate): AbstractCriterionConditions(id, playerPredicate)
}