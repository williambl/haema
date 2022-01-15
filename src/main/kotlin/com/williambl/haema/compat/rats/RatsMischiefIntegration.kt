package com.williambl.haema.compat.rats

import com.williambl.haema.ability.component.invisibility.EntityInvisibilityAbilityComponent
import com.williambl.haema.ability.component.invisibility.InvisibilityAbilityComponent
import com.williambl.haema.ability.component.strength.EntityStrengthAbilityComponent
import com.williambl.haema.ability.component.strength.StrengthAbilityComponent
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.api.VampireConversionEvents
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.component.EntityVampireComponent
import com.williambl.haema.isVampire
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy
import ladysnake.ratsmischief.common.entity.RatEntity
import net.fabricmc.fabric.api.gamerule.v1.GameRuleFactory
import net.fabricmc.fabric.api.gamerule.v1.GameRuleRegistry
import net.minecraft.text.LiteralText
import net.minecraft.util.Formatting
import net.minecraft.world.GameRules

lateinit var ratsCanConvertPlayers: GameRules.Key<GameRules.BooleanRule>

fun initRatsMischiefIntegration() {
    BloodDrinkingEvents.ON_BLOOD_DRINK.register(BloodDrinkingEvents.DrinkBloodEvent { drinker, target, world ->
        if (target is RatEntity) {
            (target).isVampire = true
        }
    })
    VampireConversionEvents.CONVERT.register(VampireConversionEvents.ConversionEvent {
        if (it is RatEntity && !it.hasCustomName()) {
            it.customName = LiteralText(if (it.random.nextFloat() < 0.02) if (it.random.nextBoolean()) "Count D-Rat-Cula" else "Capri-Sun" else "VampiRat")
                .formatted(Formatting.DARK_RED)
        }
    })

    ratsCanConvertPlayers = GameRuleRegistry.register("ratsCanConvertPlayers", GameRules.Category.PLAYER, GameRuleFactory.createBooleanRule(false))
}

fun registerRatVampireComponent(registry: EntityComponentFactoryRegistry) {
    registry.registerFor(RatEntity::class.java, VampireComponent.entityKey, ::EntityVampireComponent)
    registry.registerFor(RatEntity::class.java, InvisibilityAbilityComponent.entityKey, ::EntityInvisibilityAbilityComponent)
    registry.registerFor(RatEntity::class.java, StrengthAbilityComponent.entityKey, ::EntityStrengthAbilityComponent)
}