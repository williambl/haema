package com.williambl.haema.common.item

import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.util.addBlood
import com.williambl.haema.common.util.giveVampiricStrength
import net.minecraft.client.util.ITooltipFlag
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.potion.Potion
import net.minecraft.util.ActionResult
import net.minecraft.util.DamageSource
import net.minecraft.util.EnumActionResult
import net.minecraft.util.EnumHand
import net.minecraft.world.World
import net.minecraftforge.event.RegistryEvent
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent

@Mod.EventBusSubscriber()
object ModItems {

    @SubscribeEvent
    @JvmStatic
    fun registerItems(event: RegistryEvent.Register<Item>) {
        event.registry.registerAll(
                object : Item() {
                    override fun addInformation(stack: ItemStack, worldIn: World?, tooltip: MutableList<String>, flagIn: ITooltipFlag) {
                        super.addInformation(stack, worldIn, tooltip, flagIn)
                        tooltip.add("An ancient bottle of blood from an extinct race...")
                    }
                }.setRegistryName("vampire_blood_vial").setUnlocalizedName("vampire_blood_vial").setCreativeTab(CreativeTabs.MISC),
                object : Item() {
                    override fun addInformation(stack: ItemStack, worldIn: World?, tooltip: MutableList<String>, flagIn: ITooltipFlag) {
                        super.addInformation(stack, worldIn, tooltip, flagIn)
                        tooltip.add("It's just begging to be injected...")
                    }

                    override fun onItemRightClick(worldIn: World, playerIn: EntityPlayer, handIn: EnumHand): ActionResult<ItemStack> {
                        if (worldIn.isRemote)
                            return ActionResult(EnumActionResult.PASS, playerIn.getHeldItem(handIn))
                        val vampirismCap = playerIn.getCapability(VampirismProvider.vampirism!!, null)!!
                        if (!vampirismCap.isVampire()) {
                            if (playerIn.getActivePotionEffect(Potion.getPotionFromResourceLocation("minecraft:weakness")!!) != null) {
                                vampirismCap.setIsVampire(true)
                                playerIn.addBlood(0.5f)
                                playerIn.giveVampiricStrength(200, 5)
                            } else {
                                playerIn.attackEntityFrom(DamageSource.MAGIC, 20.0f)
                            }
                        }
                        playerIn.getHeldItem(handIn).count = 0
                        println(vampirismCap.isVampire())
                        return ActionResult(EnumActionResult.SUCCESS, playerIn.getHeldItem(handIn))
                    }
                }.setRegistryName("vampire_blood_syringe").setUnlocalizedName("vampire_blood_syringe").setCreativeTab(CreativeTabs.MISC)
        )
    }
}