package com.williambl.haema.blood

import com.williambl.haema.convert
import com.williambl.haema.criteria.VampireConversionFailureCriterion
import com.williambl.haema.damagesource.incompatibleBlood
import com.williambl.haema.isVampire
import com.williambl.haema.util.HaemaGameRules
import com.williambl.haema.vampireComponent
import net.minecraft.block.DispenserBlock
import net.minecraft.block.dispenser.FallibleItemDispenserBehavior
import net.minecraft.client.item.TooltipContext
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.text.Text
import net.minecraft.util.Formatting
import net.minecraft.util.Hand
import net.minecraft.util.TypedActionResult
import net.minecraft.util.math.BlockPointer
import net.minecraft.util.math.Box
import net.minecraft.world.World

class VampireBloodInjectorItem(settings: Settings?) : Item(settings) {
    override fun use(world: World, user: PlayerEntity, hand: Hand): TypedActionResult<ItemStack> {
        val emptyStack = ItemStack(BloodModule.EMPTY_VAMPIRE_BLOOD_INJECTOR)
        return if (tryUse(user, hand))
            TypedActionResult.consume(emptyStack)
        else
            TypedActionResult.pass(user.getStackInHand(hand))
    }

    override fun appendTooltip(stack: ItemStack?, world: World?, tooltip: MutableList<Text>, context: TooltipContext?) {
        super.appendTooltip(stack, world, tooltip, context)
        tooltip.add(Text.translatable("item.haema.vampire_blood_injector.desc").formatted(Formatting.DARK_RED))
    }

    fun tryUse(user: PlayerEntity, hand: Hand? = null): Boolean {
        if (user.world.isClient) {
            return false
        }

        if ((user).isVampire) {
            (user.vampireComponent).addBlood(6.0)
            return true
        }

        if (!user.hasStatusEffect(StatusEffects.STRENGTH) || user.getStatusEffect(StatusEffects.STRENGTH)!!.amplifier <= 0) {
            VampireConversionFailureCriterion.trigger(user as ServerPlayerEntity)
            //awful hack, but the player dies before the item can be changed
            if (hand != null)
                user.setStackInHand(hand, ItemStack(BloodModule.EMPTY_VAMPIRE_BLOOD_INJECTOR))

            user.addStatusEffect(StatusEffectInstance(StatusEffects.WITHER, 3000))
            user.addStatusEffect(StatusEffectInstance(StatusEffects.NAUSEA, 100))
            user.damage(user.damageSources.incompatibleBlood(), 20f)
            return true
        }

        if (!user.world.gameRules[HaemaGameRules.playerVampireConversion].get()) {
            user.sendMessage(Text.translatable("gui.haema.message.conversion_blocked_by_gamerule"), true)
            return false
        }

        convert(user)
        return true
    }

    companion object DispenserBehavior : FallibleItemDispenserBehavior() {
        override fun dispenseSilently(pointer: BlockPointer, stack: ItemStack): ItemStack {
            val blockPos = pointer.pos.offset(pointer.blockState.get(DispenserBlock.FACING))
            val user = pointer.world.getEntitiesByClass(PlayerEntity::class.java, Box(blockPos), null)
                .firstOrNull() ?: return stack
            return if ((stack.item as VampireBloodInjectorItem).tryUse(user))
                ItemStack(BloodModule.EMPTY_VAMPIRE_BLOOD_INJECTOR)
            else
                stack
        }
    }
}
