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
        if ((player as Vampirable).isVampire) {
            if (entity is LivingEntity)
                (player.hungerManager as VampireBloodManager).feed(entity, player)
            else
                ActionResult.PASS
        }
        else ActionResult.PASS
    })

    SunlightSicknessEffect.instance = SunlightSicknessEffect(StatusEffectType.HARMFUL, 245 shl 24 or 167 shl 16 or 66 shl 8)
            .addAttributeModifier(
                    EntityAttributes.GENERIC_ATTACK_DAMAGE,
                    "c85d1cfe-2c10-4d25-b650-49c045979842",
                    -4.0,
                    EntityAttributeModifier.Operation.ADDITION
            )
    VampiricStrengthEffect.instance = VampiricStrengthEffect(StatusEffectType.BENEFICIAL, 171 shl 24 or 12 shl 16 or 12 shl 8)
            .addAttributeModifier(
                    EntityAttributes.GENERIC_ATTACK_DAMAGE,
                    "beb69dfa-5de3-4f82-82a5-29f5ba715a18",
                    4.0,
                    EntityAttributeModifier.Operation.ADDITION
            )
            .addAttributeModifier(
                    EntityAttributes.GENERIC_ATTACK_SPEED,
                    "3ca8311c-601a-44f9-97ee-2b0677247e64",
                    0.3,
                    EntityAttributeModifier.Operation.MULTIPLY_TOTAL
            )
            .addAttributeModifier(
                    EntityAttributes.GENERIC_MAX_HEALTH,
                    "858a6a28-5092-49ea-a94e-eb74db018a92",
                    5.0,
                    EntityAttributeModifier.Operation.ADDITION
            )
            .addAttributeModifier(
                    EntityAttributes.GENERIC_MOVEMENT_SPEED,
                    "7a47b1b8-16a5-4877-905a-07ffd5d2189b",
                    0.2,
                    EntityAttributeModifier.Operation.MULTIPLY_TOTAL
            )
    VampiricWeaknessEffect.instance = VampiricWeaknessEffect(StatusEffectType.HARMFUL, 171 shl 24 or 12 shl 16 or 12 shl 8)
            .addAttributeModifier(
                    EntityAttributes.GENERIC_ATTACK_DAMAGE,
                    "beb69dfa-5de3-4f82-82a5-29f5ba715a18",
                    -4.0,
                    EntityAttributeModifier.Operation.ADDITION
            )
            .addAttributeModifier(
                    EntityAttributes.GENERIC_ATTACK_SPEED,
                    "3ca8311c-601a-44f9-97ee-2b0677247e64",
                    -0.25,
                    EntityAttributeModifier.Operation.MULTIPLY_TOTAL
            )
            .addAttributeModifier(
                    EntityAttributes.GENERIC_MAX_HEALTH,
                    "858a6a28-5092-49ea-a94e-eb74db018a92",
                    -2.0,
                    EntityAttributeModifier.Operation.ADDITION
            )
            .addAttributeModifier(
                    EntityAttributes.GENERIC_MOVEMENT_SPEED,
                    "7a47b1b8-16a5-4877-905a-07ffd5d2189b",
                    -0.15,
                    EntityAttributeModifier.Operation.MULTIPLY_TOTAL
            )

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

