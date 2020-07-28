package com.williambl.haema.item

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.damagesource.IncompatibleBloodDamageSource
import net.minecraft.client.item.TooltipContext
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.util.Hand
import net.minecraft.util.Identifier
import net.minecraft.util.TypedActionResult
import net.minecraft.util.registry.Registry
import net.minecraft.world.World

class VampireBloodInjectorItem(settings: Settings?) : Item(settings) {

    override fun use(world: World, user: PlayerEntity, hand: Hand): TypedActionResult<ItemStack> {
        val emptyStack = ItemStack(Registry.ITEM.get(Identifier("haema:empty_vampire_blood_injector")))
        return if (tryUse(user))
            TypedActionResult.consume(emptyStack)
        else
            TypedActionResult.pass(user.getStackInHand(hand))
    }

    override fun appendTooltip(stack: ItemStack?, world: World?, tooltip: MutableList<Text>, context: TooltipContext?) {
        super.appendTooltip(stack, world, tooltip, context)
        tooltip.add(TranslatableText("item.haema.vampire_blood_injector.desc").formatted(Formatting.DARK_RED))
    }

    fun tryUse(user: PlayerEntity): Boolean {
        if ((user as Vampirable).isVampire) {
            (user.hungerManager as VampireBloodManager).addBlood(6.0)
            return true
        }

        if (!user.hasStatusEffect(StatusEffects.STRENGTH) || user.getStatusEffect(StatusEffects.STRENGTH)!!.amplifier <= 0) {
            user.addStatusEffect(StatusEffectInstance(StatusEffects.WITHER, 3000))
            user.addStatusEffect(StatusEffectInstance(StatusEffects.NAUSEA, 100))
            user.damage(IncompatibleBloodDamageSource.instance, 20f)
            return true
        }

        Vampirable.convert(user)
        return true
    }
}