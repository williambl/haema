package com.williambl.haema.ritual

import com.williambl.haema.Vampirable
import com.williambl.haema.VampireAbility
import com.williambl.haema.craft.ritual.RitualInventory
import com.williambl.haema.ritualTable
import net.fabricmc.fabric.api.screenhandler.v1.ScreenHandlerRegistry
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.network.PacketByteBuf
import net.minecraft.screen.PropertyDelegate
import net.minecraft.screen.ScreenHandler
import net.minecraft.screen.ScreenHandlerContext
import net.minecraft.screen.ScreenHandlerType
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry

class RitualTableScreenHandler(syncId: Int, val inv: RitualInventory, val context: ScreenHandlerContext)
    : ScreenHandler(ritualTableScreenHandlerType, syncId) {

    constructor(syncId: Int, playerInventory: PlayerInventory, packetByteBuf: PacketByteBuf) : this(
        syncId,
        RitualInventory(
            listOf(),
            Registry.FLUID.get(packetByteBuf.readIdentifier()),
            packetByteBuf.readBlockPos(),
            playerInventory.player,
            packetByteBuf.readInt()
        ),
        ScreenHandlerContext.EMPTY
    )

    val propertyDelegate = object : PropertyDelegate {
        val abilities = VampireAbility.values()
        val player = (inv.player as Vampirable)

        override fun size(): Int = abilities.size

        override fun get(index: Int): Int = player.getAbilityLevel(abilities[index])

        override fun set(index: Int, value: Int) {
            player.setAbilityLevel(abilities[index], value)
        }
    }

    init {
        addProperties(propertyDelegate)
    }

    fun getProperty(index: Int): Int = propertyDelegate[index]

    override fun canUse(player: PlayerEntity): Boolean = canUse(context, player, ritualTable)

    companion object {
        val ritualTableScreenHandlerType: ScreenHandlerType<RitualTableScreenHandler>
                = ScreenHandlerRegistry.registerExtended(Identifier("haema:ritual_table")) {
                i: Int, inv: PlayerInventory, buf: PacketByteBuf -> RitualTableScreenHandler(i, inv, buf)
        }
    }
}