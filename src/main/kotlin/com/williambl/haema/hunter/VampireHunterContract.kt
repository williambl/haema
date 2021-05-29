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
        ServerEntityCombatEvents.AFTER_KILLED_OTHER_ENTITY.register { world, entity, target ->
            if (entity is PlayerEntity && target is Vampirable && target.isVampire) {
                (0 until entity.inventory.size()).asSequence().map { entity.inventory.getStack(it) }
                    .find { it.item == this && !it.isContractFulfilled() }
                    ?.fulfilContract()
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
        if (stack.isContractFulfilled()) {
            tooltip.add(TranslatableText("$translationKey.fulfilled").formatted(Formatting.AQUA))
        } else {
            tooltip.add(TranslatableText("$translationKey.unfulfilled").formatted(Formatting.RED))
        }
        super.appendTooltip(stack, world, tooltip, context)
    }

    override fun hasGlint(stack: ItemStack): Boolean {
        return super.hasGlint(stack) || stack.isContractFulfilled()
    }
}

@Suppress("UsePropertyAccessSyntax")
fun ItemStack.isContractFulfilled() = this.getOrCreateTag().getBoolean("ContractFulfilled")

@Suppress("UsePropertyAccessSyntax")
fun ItemStack.fulfilContract() = this.getOrCreateTag().putBoolean("ContractFulfilled", true)