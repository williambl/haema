package com.williambl.haema.item

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireBloodManager
import com.williambl.haema.util.playerVampireConversion
import net.minecraft.client.item.TooltipContext
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

class EmptyVampireBloodInjectorItem(settings: Settings?) : Item(settings) {

    override fun use(world: World, user: PlayerEntity, hand: Hand): TypedActionResult<ItemStack> {
        return if (tryUse(user, hand))
            TypedActionResult.consume(ItemStack(Registry.ITEM.get(Identifier("haema:vampire_blood_injector"))))
        else
            TypedActionResult.pass(user.getStackInHand(hand))
    }

    override fun appendTooltip(stack: ItemStack?, world: World?, tooltip: MutableList<Text>, context: TooltipContext?) {
        super.appendTooltip(stack, world, tooltip, context)
        tooltip.add(TranslatableText("item.haema.empty_vampire_blood_injector.desc").formatted(Formatting.DARK_RED))
    }

    fun tryUse(user: PlayerEntity, hand: Hand? = null): Boolean {
        if ((user as Vampirable).isVampire) {
            if (user.hasStatusEffect(StatusEffects.WEAKNESS) && !(user.isPermanentVampire) && user.world.gameRules[playerVampireConversion].get()) {
                (user as Vampirable).isVampire = false
                //awful hack, but the player dies before the item can be changed
                if (hand != null)
                    user.setStackInHand(hand, ItemStack(Registry.ITEM.get(Identifier("haema:vampire_blood_injector"))))
                user.kill()
                return true
            }
            if ((user.hungerManager as VampireBloodManager).absoluteBloodLevel < 6.0) {
                (user.hungerManager as VampireBloodManager).removeBlood(6.0)
                return false
            }
            (user.hungerManager as VampireBloodManager).removeBlood(6.0)
            return true
        }

        return false
    }
}
