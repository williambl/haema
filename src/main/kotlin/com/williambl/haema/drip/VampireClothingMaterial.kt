package com.williambl.haema.drip

import net.minecraft.entity.EquipmentSlot
import net.minecraft.item.ArmorMaterial
import net.minecraft.item.ArmorMaterials
import net.minecraft.item.Items
import net.minecraft.recipe.Ingredient
import net.minecraft.sound.SoundEvent

object VampireClothingMaterial: ArmorMaterial {
    override fun getDurability(slot: EquipmentSlot?): Int = ArmorMaterials.CHAIN.getDurability(slot)

    override fun getProtectionAmount(slot: EquipmentSlot?): Int = ArmorMaterials.LEATHER.getProtectionAmount(slot)

    override fun getEnchantability(): Int = ArmorMaterials.CHAIN.enchantability

    override fun getEquipSound(): SoundEvent = ArmorMaterials.LEATHER.equipSound

    override fun getRepairIngredient(): Ingredient = Ingredient.ofItems(Items.LEATHER)

    override fun getName(): String = "vampire_clothing"

    override fun getToughness(): Float = ArmorMaterials.CHAIN.toughness

    override fun getKnockbackResistance(): Float = ArmorMaterials.CHAIN.knockbackResistance
}