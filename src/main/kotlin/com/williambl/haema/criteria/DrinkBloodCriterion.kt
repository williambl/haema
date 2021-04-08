package com.williambl.haema.criteria

import com.google.gson.JsonObject
import net.minecraft.advancement.criterion.AbstractCriterion
import net.minecraft.advancement.criterion.AbstractCriterionConditions
import net.minecraft.entity.Entity
import net.minecraft.loot.context.LootContext
import net.minecraft.predicate.entity.AdvancementEntityPredicateDeserializer
import net.minecraft.predicate.entity.EntityPredicate
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.Identifier

object DrinkBloodCriterion: AbstractCriterion<DrinkBloodCriterion.Conditions>() {
    private val id = Identifier("haema:drink_blood")
    override fun getId(): Identifier = id

    override fun conditionsFromJson(
        obj: JsonObject,
        playerPredicate: EntityPredicate.Extended,
        predicateDeserializer: AdvancementEntityPredicateDeserializer
    ): Conditions = Conditions(playerPredicate, EntityPredicate.Extended.getInJson(obj, "entity", predicateDeserializer))

    fun trigger(player: ServerPlayerEntity, target: Entity) {
        this.test(player) { it.matches(EntityPredicate.createAdvancementEntityLootContext(player, target)) }
    }

    class Conditions(playerPredicate: EntityPredicate.Extended, private val targetPredicate: EntityPredicate.Extended):
        AbstractCriterionConditions(id, playerPredicate) {
            fun matches(target: LootContext): Boolean = targetPredicate.test(target)
    }
}