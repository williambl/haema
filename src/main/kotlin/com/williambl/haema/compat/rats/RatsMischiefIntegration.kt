package com.williambl.haema.compat.rats

import com.williambl.haema.Vampirable
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.component.VampirePlayerComponent
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import ladysnake.ratsmischief.common.entity.RatEntity
import net.fabricmc.fabric.api.gamerule.v1.GameRuleFactory
import net.fabricmc.fabric.api.gamerule.v1.GameRuleRegistry
import net.minecraft.world.GameRules

lateinit var ratsCanConvertPlayers: GameRules.Key<GameRules.BooleanRule>

fun initRatsMischiefIntegration() {
    BloodDrinkingEvents.ON_BLOOD_DRINK.register(BloodDrinkingEvents.DrinkBloodEvent { drinker, target, world ->
        if (target is RatEntity) {
            (target as Vampirable).isVampire = true
        }
    })
    ratsCanConvertPlayers = GameRuleRegistry.register("ratsCanConvertPlayers", GameRules.Category.PLAYER, GameRuleFactory.createBooleanRule(false))
}

fun registerRatVampireComponent(registry: EntityComponentFactoryRegistry) {
    registry.registerFor(RatEntity::class.java, VampireComponent.entityKey) { entity -> VampirePlayerComponent(entity) }
}