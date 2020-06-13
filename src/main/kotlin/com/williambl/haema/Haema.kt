package com.williambl.haema

import net.fabricmc.fabric.api.event.player.UseBlockCallback
import net.fabricmc.fabric.api.event.player.UseEntityCallback
import net.fabricmc.fabric.api.network.PacketRegistry
import net.fabricmc.fabric.api.tag.FabricTagBuilder
import net.fabricmc.fabric.api.tag.TagRegistry
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffectType
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.item.Items
import net.minecraft.util.ActionResult
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry

val bloodLevelPackeId = Identifier("haema:bloodlevelsync")

val goodBloodTag = TagRegistry.entityType(Identifier("haema:good_blood_sources"))
val mediumBloodTag = TagRegistry.entityType(Identifier("haema:medium_blood_sources"))
val poorBloodTag = TagRegistry.entityType(Identifier("haema:poor_blood_sources"))

fun init() {
    UseBlockCallback.EVENT.register(UseBlockCallback { playerEntity, world, hand, blockHitResult ->
        if (playerEntity.getStackInHand(hand).item == Items.STICK) {
            (playerEntity.hungerManager as VampireBloodManager).addBlood(0.1)
        } else if (playerEntity.getStackInHand(hand).item == Items.GOLD_NUGGET) {
            println((playerEntity.hungerManager as VampireBloodManager).getBloodLevel())
        }
        ActionResult.PASS
    })

    UseEntityCallback.EVENT.register(UseEntityCallback { player, world, hand, entity, entityHitResult ->
        if (entity is LivingEntity)
            (player.hungerManager as VampireBloodManager).feed(entity, player)
        else
            ActionResult.PASS
    })

    SunlightSicknessEffect.instance = SunlightSicknessEffect(StatusEffectType.HARMFUL, 245 shl 24 or 167 shl 16 or 66 shl 8)
        .addAttributeModifier(
            EntityAttributes.GENERIC_ATTACK_DAMAGE,
            "c85d1cfe-2c10-4d25-b650-49c045979842",
            -4.0,
            EntityAttributeModifier.Operation.ADDITION
        )

    Registry.register(
        Registry.STATUS_EFFECT,
        Identifier("haema:sunlight_sickness"),
        SunlightSicknessEffect.instance
    )
}

