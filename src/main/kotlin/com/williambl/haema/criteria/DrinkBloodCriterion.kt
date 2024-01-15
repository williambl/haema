package com.williambl.haema.criteria

import com.google.gson.JsonObject
import com.williambl.haema.id
import net.minecraft.advancement.criterion.AbstractCriterion
import net.minecraft.advancement.criterion.AbstractCriterionConditions
import net.minecraft.entity.Entity
import net.minecraft.loot.context.LootContext
import net.minecraft.predicate.entity.AdvancementEntityPredicateDeserializer
import net.minecraft.predicate.entity.EntityPredicate
import net.minecraft.predicate.entity.LootContextPredicate
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.Identifier

object DrinkBloodCriterion: AbstractCriterion<DrinkBloodCriterion.Conditions>() {
    private val id = id("drink_blood")
    override fun getId(): Identifier = id

    override fun conditionsFromJson(
        obj: JsonObject,
        playerPredicate: LootContextPredicate,
        predicateDeserializer: AdvancementEntityPredicateDeserializer
    ): Conditions = Conditions(playerPredicate, EntityPredicate.contextPredicateFromJson(obj, "entity", predicateDeserializer))

    fun trigger(player: ServerPlayerEntity, target: Entity) {
        this.trigger(player) { it.matches(EntityPredicate.createAdvancementEntityLootContext(player, target)) }
    }

    class Conditions(playerPredicate: LootContextPredicate, private val targetPredicate: LootContextPredicate):
        AbstractCriterionConditions(id, playerPredicate) {
            fun matches(target: LootContext): Boolean = targetPredicate.test(target)
    }
}