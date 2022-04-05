package com.williambl.haema.spells

import net.minecraft.client.item.TooltipContext
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.text.Text
import net.minecraft.util.Formatting
import net.minecraft.util.Hand
import net.minecraft.util.TypedActionResult
import net.minecraft.world.World

class SpellScrollItem(private val spell: Spell, settings: Settings): Item(settings) {
    override fun use(world: World, user: PlayerEntity, hand: Hand): TypedActionResult<ItemStack> {
        spell.use(world, user, hand)

        return TypedActionResult.success(user.getStackInHand(hand).also { it.decrement(1) }, false)
    }

    override fun appendTooltip(
        stack: ItemStack,
        world: World?,
        tooltip: MutableList<Text>,
        context: TooltipContext?
    ) {
        tooltip.add(SpellsModule.DISSOLUTION.getName().formatted(Formatting.AQUA))
        super.appendTooltip(stack, world, tooltip, context)
    }

    override fun hasGlint(stack: ItemStack): Boolean {
        return true
    }
}
