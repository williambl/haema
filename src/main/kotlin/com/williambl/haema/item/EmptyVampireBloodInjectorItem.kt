package com.williambl.haema.item

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.damagesource.IncompatibleBloodDamageSource
import net.minecraft.client.item.TooltipContext
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.*
import net.minecraft.util.registry.Registry
import net.minecraft.world.World

class EmptyVampireBloodInjectorItem(settings: Settings?) : Item(settings) {

    override fun use(world: World, user: PlayerEntity, hand: Hand): TypedActionResult<ItemStack> {
        if ((user as Vampirable).isVampire) {
            if (user.hasStatusEffect(StatusEffects.WEAKNESS)) {
                (user as Vampirable).isVampire = false
                user.kill()
            }
            if ((user.hungerManager as VampireBloodManager).absoluteBloodLevel < 6.0)
                return TypedActionResult.consume(user.getStackInHand(hand))
            (user.hungerManager as VampireBloodManager).removeBlood(6.0)
            return TypedActionResult.consume(ItemStack(Registry.ITEM.get(Identifier("haema:vampire_blood_injector"))))
        }

        return TypedActionResult.pass(user.getStackInHand(hand))
    }

    override fun appendTooltip(stack: ItemStack?, world: World?, tooltip: MutableList<Text>, context: TooltipContext?) {
        super.appendTooltip(stack, world, tooltip, context)
        tooltip.add(TranslatableText("item.haema.empty_vampire_blood_injector.desc").formatted(Formatting.DARK_RED))
    }
}