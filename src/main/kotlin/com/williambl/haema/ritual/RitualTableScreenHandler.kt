package com.williambl.haema.ritual

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.id
import com.williambl.haema.ritual.craft.RitualInventory
import com.williambl.haema.setAbilityLevel
import io.netty.buffer.Unpooled
import net.fabricmc.api.EnvType
import net.fabricmc.api.Environment
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking
import net.fabricmc.fabric.api.screenhandler.v1.ExtendedScreenHandlerFactory
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.network.PacketByteBuf
import net.minecraft.screen.PropertyDelegate
import net.minecraft.screen.ScreenHandler
import net.minecraft.screen.ScreenHandlerContext
import net.minecraft.server.network.ServerPlayerEntity
import net.minecraft.text.LiteralText
import net.minecraft.text.Text
import net.minecraft.util.registry.Registry

class RitualTableScreenHandler(syncId: Int, val inv: RitualInventory, private val context: ScreenHandlerContext)
    : ScreenHandler(RitualModule.RITUAL_TABLE_SCREEN_HANDLER, syncId) {

    constructor(syncId: Int, playerInventory: PlayerInventory, packetByteBuf: PacketByteBuf) : this(
        syncId,
        RitualInventory(
            listOf(),
            Registry.FLUID.get(packetByteBuf.readIdentifier()),
            packetByteBuf.readBlockPos(),
            playerInventory.player,
            packetByteBuf.readVarInt()
        ),
        ScreenHandlerContext.EMPTY
    )

    private val propertyDelegate = object : PropertyDelegate {
        val abilities =
            AbilityModule.ABILITY_REGISTRY.entrySet.associate { AbilityModule.ABILITY_REGISTRY.getRawId(it.value) to it.value }

        override fun size(): Int = abilities.size

        override fun get(index: Int): Int {
            return inv.player.getAbilityLevel(abilities[index] ?: return 0)
        }

        override fun set(index: Int, value: Int) {
            inv.player.setAbilityLevel(abilities[index] ?: return, value)
        }
    }

    init {
        addProperties(propertyDelegate)
    }

    fun getProperty(index: Int): Int = propertyDelegate[index]

    @Environment(EnvType.CLIENT)
    fun transferLevels(amount: Int, from: Int, to: Int) {
        ClientPlayNetworking.send(id("transferlevels"), PacketByteBuf(Unpooled.buffer()).writeVarInt(syncId).writeVarInt(amount).writeVarInt(from).writeVarInt(to))
    }

    override fun canUse(player: PlayerEntity): Boolean = canUse(context, player, RitualTable.instance)

    class Factory(private val inv: RitualInventory): ExtendedScreenHandlerFactory {
        override fun createMenu(syncId: Int, playerInv: PlayerInventory, player: PlayerEntity): ScreenHandler {
            return RitualTableScreenHandler(syncId, inv, ScreenHandlerContext.create(inv.player.world, inv.pos))
        }

        override fun getDisplayName(): Text {
            return LiteralText("Ritual Table")
        }

        override fun writeScreenOpeningData(player: ServerPlayerEntity, buf: PacketByteBuf) {
            buf.writeIdentifier(Registry.FLUID.getId(inv.fluid))
            buf.writeBlockPos(inv.pos)
            buf.writeVarInt(inv.level)
        }
    }
}