package com.williambl.haema.hunter

import com.mojang.authlib.GameProfile
import com.williambl.haema.isVampire
import net.fabricmc.api.EnvType
import net.fabricmc.api.Environment
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityCombatEvents
import net.minecraft.client.item.TooltipContext
import net.minecraft.entity.Entity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NbtCompound
import net.minecraft.nbt.NbtHelper
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.text.Text
import net.minecraft.util.Formatting
import net.minecraft.world.World

class VampireHunterContract(settings: Settings): Item(settings) {
    init {
        ServerEntityCombatEvents.AFTER_KILLED_OTHER_ENTITY.register { world, entity, target ->
            if (entity is PlayerEntity && target.isVampire) {
                (0 until entity.inventory.size()).asSequence().map { entity.inventory.getStack(it) }
                    .find {
                            it.item == this &&
                            !it.isContractFulfilled() &&
                            it.getContractTarget()?.let { profile -> target.uuid == profile.id } ?: true
                    }
                    ?.fulfilContract(entity)
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
            tooltip.add(Text.translatable("$translationKey.fulfilled").formatted(Formatting.AQUA))
        } else {
            tooltip.add(Text.translatable("$translationKey.unfulfilled").formatted(Formatting.RED))
        }
        stack.getContractTarget()?.let {
            tooltip.add(Text.translatable("$translationKey.target", it.name))
        }
        super.appendTooltip(stack, world, tooltip, context)
    }

    override fun hasGlint(stack: ItemStack): Boolean {
        return super.hasGlint(stack) || stack.isContractFulfilled()
    }
}

@Suppress("UsePropertyAccessSyntax")
fun ItemStack.setContractTarget(target: PlayerEntity) = this.getOrCreateNbt().put("ContractTarget", NbtHelper.writeGameProfile(NbtCompound(), target.gameProfile))

@Suppress("UsePropertyAccessSyntax")
fun ItemStack.getContractTarget(): GameProfile? = this.getOrCreateNbt().get("ContractTarget")?.let { NbtHelper.toGameProfile(it as NbtCompound) }

@Suppress("UsePropertyAccessSyntax")
fun ItemStack.isContractFulfilled() = this.getOrCreateNbt().getBoolean("ContractFulfilled")

@Suppress("UsePropertyAccessSyntax")
fun ItemStack.fulfilContract(holder: Entity) {
    this.getOrCreateNbt().putBoolean("ContractFulfilled", true)
    holder.world.playSound(holder.x, holder.y, holder.z, SoundEvents.UI_TOAST_CHALLENGE_COMPLETE, SoundCategory.NEUTRAL, 1.0f, 1.0f, false)
}
