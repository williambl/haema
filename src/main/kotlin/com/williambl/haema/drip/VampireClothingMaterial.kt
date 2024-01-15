package com.williambl.haema.drip

import net.minecraft.item.ArmorItem
import net.minecraft.item.ArmorMaterial
import net.minecraft.item.ArmorMaterials
import net.minecraft.item.Items
import net.minecraft.recipe.Ingredient
import net.minecraft.sound.SoundEvent

object VampireClothingMaterial: ArmorMaterial {
    override fun getDurability(type: ArmorItem.Type?): Int = ArmorMaterials.CHAIN.getDurability(type)

    override fun getProtection(type: ArmorItem.Type?): Int = ArmorMaterials.LEATHER.getProtection(type)

    override fun getEnchantability(): Int = ArmorMaterials.CHAIN.enchantability

    override fun getEquipSound(): SoundEvent = ArmorMaterials.LEATHER.equipSound

    override fun getRepairIngredient(): Ingredient = Ingredient.ofItems(Items.LEATHER)

    override fun getName(): String = "vampire_clothing"

    override fun getToughness(): Float = ArmorMaterials.CHAIN.toughness

    override fun getKnockbackResistance(): Float = ArmorMaterials.CHAIN.knockbackResistance
}