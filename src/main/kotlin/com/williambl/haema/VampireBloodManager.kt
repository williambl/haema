package com.williambl.haema

import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.player.HungerManager
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.world.GameRules

class VampireBloodManager : HungerManager() {

    //TODO: make this have effect
    var canSprint = false

    override fun update(player: PlayerEntity?) {
        player!!

        canSprint = foodLevel >= 6
        player.isSilent = (foodLevel >= 10 && player.isSprinting) || foodLevel >= 12

        if (foodLevel <= 3) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.WEAKNESS, 1, 4-foodLevel))
        }

        if (foodLevel >= 6) {
            //TODO: extended reach
        }

        if (foodLevel >= 8) {
            if (player.world.gameRules.get(GameRules.NATURAL_REGENERATION).get() && player.canFoodHeal()) {
                player.heal(1.0f)
            }
        }

        if (foodLevel >= 10) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.STRENGTH, 1, 1))
            //TODO: faster sprinting
        }

        if (foodLevel >= 12) {
            //TODO: shader
        }

        if (foodLevel >= 14) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.STRENGTH, 1, 2))
        }

        if (foodLevel >= 15) {
            //TODO: shader part 2 electric boogaloo
        }

        if (foodLevel >= 18) {
            //TODO: dash/flight like yoshi but fast
        }

        if (foodLevel >= 19) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.STRENGTH, 1, 3))
        }

        if (foodLevel >= 20) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.INVISIBILITY, 1, 1))
        }
    }
}
