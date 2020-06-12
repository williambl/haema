package com.williambl.haema

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.player.HungerManager
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.world.GameRules
import java.util.*

class VampireBloodManager : HungerManager() {

    companion object {
        private val VAMPIRE_REACH_UUID = UUID.fromString("0eb4fc5f-71d5-4440-b517-bcc18e1df6f4")
        private val VAMPIRE_ATTACK_RANGE_UUID = UUID.fromString("3267a46b-2b48-429f-a3a8-439aa87a876d")
        private val VAMPIRE_REACH = EntityAttributeModifier(VAMPIRE_REACH_UUID, "Vampire reach extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
        private val VAMPIRE_ATTACK_RANGE = EntityAttributeModifier(VAMPIRE_ATTACK_RANGE_UUID, "Vampire attack range extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
    }

    //TODO: make this have effect
    var canSprint = false

    override fun update(player: PlayerEntity?) {
        player!!

        canSprint = foodLevel >= 6
        player.isSilent = (foodLevel >= 10 && player.isSprinting) || foodLevel >= 12

        if (foodLevel <= 3) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.WEAKNESS, 1, 4-foodLevel))
        }

        val reachAttr = player.getAttributeInstance(ReachEntityAttributes.REACH)
        val attackRangeAttr = player.getAttributeInstance(ReachEntityAttributes.ATTACK_RANGE)

        if (foodLevel >= 6 && (reachAttr?.hasModifier(VAMPIRE_REACH) == false || attackRangeAttr?.hasModifier(VAMPIRE_ATTACK_RANGE) == false)) {
            reachAttr?.addTemporaryModifier(VAMPIRE_REACH)
            attackRangeAttr?.addTemporaryModifier(VAMPIRE_ATTACK_RANGE)
        } else if (reachAttr?.hasModifier(VAMPIRE_REACH) != false || attackRangeAttr?.hasModifier(VAMPIRE_ATTACK_RANGE) != false) {
            reachAttr?.removeModifier(VAMPIRE_REACH)
            attackRangeAttr?.removeModifier(VAMPIRE_ATTACK_RANGE)
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
