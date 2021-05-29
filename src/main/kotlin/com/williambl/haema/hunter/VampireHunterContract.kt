package com.williambl.haema.hunter

import com.williambl.haema.Vampirable
import net.fabricmc.api.EnvType
import net.fabricmc.api.Environment
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityCombatEvents
import net.minecraft.client.item.TooltipContext
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.world.World

class VampireHunterContract(settings: Settings): Item(settings) {
    init {
        ServerEntityCombatEvents.AFTER_KILLED_OTHER_ENTITY.register { world, target, entity ->
            if (entity is PlayerEntity && target is Vampirable && target.isVampire) {
                (0 until entity.inventory.size()).asSequence().map { entity.inventory.getStack(it) }
                    .find { it.item == this && !it.isComplete() }
                    ?.setComplete()
            }
        }
    }

    @Environment(EnvType.CLIENT)
    override fun appendTooltip(
        stack: ItemStack,
        world: World?,
        tooltip: MutableList<Text>,
        context: TooltipContext?
    ) {
        if (stack.isComplete()) {
            tooltip.add(TranslatableText("$translationKey.complete").formatted(Formatting.AQUA))
        } else {
            tooltip.add(TranslatableText("$translationKey.incomplete").formatted(Formatting.RED))
        }
        super.appendTooltip(stack, world, tooltip, context)
    }

    @Suppress("UsePropertyAccessSyntax")
    private fun ItemStack.isComplete() = this.getOrCreateTag().getBoolean("ContractComplete")

    @Suppress("UsePropertyAccessSyntax")
    private fun ItemStack.setComplete() = this.getOrCreateTag().putBoolean("ContractComplete", true)
}