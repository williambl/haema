package com.williambl.haema.compat.bewitchment

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.api.BloodChangeEvents
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.api.DamageSourceEfficacyEvent
import com.williambl.haema.api.client.VampireHudAddTextEvent
import moriyashiine.bewitchment.api.BewitchmentAPI
import moriyashiine.bewitchment.api.component.BloodComponent
import moriyashiine.bewitchment.api.component.TransformationComponent
import moriyashiine.bewitchment.api.event.*
import moriyashiine.bewitchment.client.BewitchmentClient
import moriyashiine.bewitchment.common.registry.BWDamageSources
import moriyashiine.bewitchment.common.registry.BWPledges
import moriyashiine.bewitchment.common.registry.BWTransformations
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import kotlin.math.ceil

fun registerBewitchmentEventListeners() {
    //
    // == Blood Syncing ==
    //

    // Sync blood Bewitchment -> Haema
    BloodSetEvents.ON_BLOOD_SET.register(BloodSetEvents.OnSetBlood { entity, amount ->
        if (entity is Vampirable && entity is PlayerEntity && entity.hungerManager is VampireBloodManager) {
            @Suppress("DEPRECATION")
            (entity.hungerManager as VampireBloodManager).absoluteBloodLevel = amount*0.2
        }
    })
    // Sync blood changes Haema -> Bewitchment
    BloodChangeEvents.ON_BLOOD_ADD.register(BloodChangeEvents.AddBloodEvent { player, amount ->
        if (BewitchmentAPI.isVampire(player, true)) {
            BloodComponent.get(player).fillBlood(ceil(amount * 5).toInt(), false)
        }
    })
    // Sync blood changes Haema -> Bewitchment
    BloodChangeEvents.ON_BLOOD_REMOVE.register(BloodChangeEvents.RemoveBloodEvent { player, amount ->
        if (BewitchmentAPI.isVampire(player, true)) {
            BloodComponent.get(player).drainBlood(ceil(amount * 5).toInt(), false)
        }
    })

    //
    // == Blood Drinking ==
    //

    // Let Bewitchment handle drinking
    BloodDrinkingEvents.CANCEL.register(BloodDrinkingEvents.CancelBloodDrinkEvent { player, world, hand, target, entityHitResult ->
        !BewitchmentAPI.isVampire(player, true)
    })
    // Get blood from mobs based on Haema's tags
    BloodSuckEvents.BLOOD_AMOUNT.register(BloodSuckEvents.SetBloodAmount { player, target, currentBloodToGive ->
        when {
            VampireBloodManager.goodBloodTag.contains(target.type) -> 5
            VampireBloodManager.mediumBloodTag.contains(target.type) -> 2
            VampireBloodManager.poorBloodTag.contains(target.type) -> 1
            else -> currentBloodToGive
        }
    })
    // Run Haema blood drink events when Bewitchment ones do
    BloodSuckEvents.ON_BLOOD_SUCK.register(BloodSuckEvents.OnBloodSuck { player, target, amount ->
        BloodDrinkingEvents.ON_BLOOD_DRINK.invoker().onDrink(player, target, player.world)
    })

    //
    // == Misc ==
    //

    // Sync vampirism status Bewitchment -> Haema
    OnTransformationSet.EVENT.register(OnTransformationSet { player, transformation ->
        val transformationComponent = TransformationComponent.get(player)
        if (transformation == BWTransformations.VAMPIRE) {
            (player as Vampirable).isVampire = true
            player.isPermanentVampire = true
        } else if (transformation != BWTransformations.VAMPIRE && transformationComponent.transformation == BWTransformations.VAMPIRE) {
            (player as Vampirable).isVampire = false
            player.isPermanentVampire = false
        }
    })
    // Let Haema handle burning
    AllowVampireBurn.EVENT.register(AllowVampireBurn { player -> false })
    // Let Haema handle healing
    AllowVampireHeal.EVENT.register(AllowVampireHeal { playerEntity, isPledgedToLilith -> false })

    // Make Bewitchment's damage sources effective against vampires
    DamageSourceEfficacyEvent.EVENT.register(DamageSourceEfficacyEvent { source, world ->
        if (source == BWDamageSources.DEATH || source == BWDamageSources.MAGIC_COPY || source == BWDamageSources.SUN || source == BWDamageSources.WEDNESDAY) {
            1.25f
        } else {
            1f
        }
    })
}

fun registerBewitchmentClientEventListeners() {
    VampireHudAddTextEvent.EVENT.register(VampireHudAddTextEvent { player, createText ->
        val texts = mutableListOf<Text>()
        if (BewitchmentAPI.isVampire(player, true)) {
            val transformationComponent = TransformationComponent.get(player)
            if (!(transformationComponent.isAlternateForm)) {
                texts.add(
                    createText(
                        BewitchmentClient.TRANSFORMATION_ABILITY.boundKeyLocalizedText.copy(),
                        true,
                        TranslatableText("compat.bewitchment.gui.haema.hud.transform")
                    )
                )
            } else {
                texts.add(
                    createText(
                        BewitchmentClient.TRANSFORMATION_ABILITY.boundKeyLocalizedText.copy(),
                        BewitchmentAPI.isPledged(player, BWPledges.LILITH) && BloodComponent.get(player).blood > 0,
                        TranslatableText("compat.bewitchment.gui.haema.hud.untransform")
                    )
                )
            }
        }
        return@VampireHudAddTextEvent texts
    })
}