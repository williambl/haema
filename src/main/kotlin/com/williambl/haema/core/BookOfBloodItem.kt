package com.williambl.haema.core

import com.williambl.haema.id
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.util.ActionResult
import net.minecraft.util.Hand
import net.minecraft.util.TypedActionResult
import net.minecraft.world.World
import vazkii.patchouli.api.PatchouliAPI
import vazkii.patchouli.common.base.PatchouliSounds
import vazkii.patchouli.common.book.BookRegistry
import vazkii.patchouli.common.item.PatchouliItems

class BookOfBloodItem(settings: Settings?) : Item(settings) {
    override fun use(world: World, player: PlayerEntity, hand: Hand): TypedActionResult<ItemStack> {
        val stack = player.getStackInHand(hand)
        val book = BookRegistry.INSTANCE.books[id("book_of_blood")]
            ?: return TypedActionResult(ActionResult.FAIL, stack)
        if (player is ServerPlayerEntity) {
            PatchouliAPI.get().openBookGUI(player, book.id)
            val sfx = PatchouliSounds.getSound(book.openSound, PatchouliSounds.BOOK_OPEN)
            player.playSound(sfx, 1f, (0.7 + Math.random() * 0.4).toFloat())
        }
        return TypedActionResult(ActionResult.SUCCESS, stack)
    }

    companion object {
        fun isBook(stack: ItemStack): Boolean {
            return stack.item is BookOfBloodItem || stack.item == PatchouliItems.BOOK
        }
    }
}
