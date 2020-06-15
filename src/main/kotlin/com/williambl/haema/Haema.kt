package com.williambl.haema

import com.williambl.haema.effect.SunlightSicknessEffect
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.effect.VampiricWeaknessEffect
import net.fabricmc.fabric.api.event.player.UseBlockCallback
import net.fabricmc.fabric.api.event.player.UseEntityCallback
import net.fabricmc.fabric.api.tag.TagRegistry
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffect
import net.minecraft.entity.effect.StatusEffectType
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.util.ActionResult
import net.minecraft.util.Hand
import net.minecraft.util.Identifier
import net.minecraft.util.TypedActionResult
import net.minecraft.util.registry.Registry
import net.minecraft.world.World

val bloodLevelPackeId = Identifier("haema:bloodlevelsync")

val goodBloodTag = TagRegistry.entityType(Identifier("haema:good_blood_sources"))
val mediumBloodTag = TagRegistry.entityType(Identifier("haema:medium_blood_sources"))
val poorBloodTag = TagRegistry.entityType(Identifier("haema:poor_blood_sources"))

val vampireEffectiveWeaponsTag = TagRegistry.item(Identifier("haema:vampire_weapons"))

fun init() {
    UseBlockCallback.EVENT.register(UseBlockCallback { player, world, hand, blockHitResult ->
        if ((player as Vampirable).isVampire) {
            when (player.getStackInHand(hand).item) {
                Items.STICK -> (player.hungerManager as VampireBloodManager).addBlood(0.1)
                Items.GOLD_NUGGET -> println((player.hungerManager as VampireBloodManager).getBloodLevel())
            }
            ActionResult.PASS
        } else ActionResult.PASS
    })

    UseEntityCallback.EVENT.register(UseEntityCallback { player, world, hand, entity, entityHitResult ->
        if ((player as Vampirable).isVampire && entity is LivingEntity && player.isSneaking)
            (player.hungerManager as VampireBloodManager).feed(entity, player)
        else ActionResult.PASS
    })

    Registry.register(
            Registry.STATUS_EFFECT,
            Identifier("haema:sunlight_sickness"),
            SunlightSicknessEffect.instance
    )

    Registry.register(
            Registry.STATUS_EFFECT,
            Identifier("haema:vampiric_strength"),
            VampiricStrengthEffect.instance
    )

    Registry.register(
            Registry.STATUS_EFFECT,
            Identifier("haema:vampiric_weakness"),
            VampiricWeaknessEffect.instance
    )

    Registry.register(
            Registry.ITEM,
            Identifier("haema:vampire_converter"),
            object : Item(Settings()) {
                override fun use(world: World?, user: PlayerEntity?, hand: Hand?): TypedActionResult<ItemStack> {
                    if (world?.isClient != false || user == null)
                        return super.use(world, user, hand)
                    Vampirable.convert(user)
                    return TypedActionResult.consume(user.getStackInHand(hand))
                }
            }
    )
}

