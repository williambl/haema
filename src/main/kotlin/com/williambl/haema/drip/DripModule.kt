package com.williambl.haema.drip

import com.williambl.haema.id
import net.fabricmc.api.ModInitializer
import net.minecraft.item.ArmorItem
import net.minecraft.item.Item
import net.minecraft.registry.Registries
import net.minecraft.registry.Registry
import net.minecraft.util.Rarity


object DripModule: ModInitializer {

    val VAMPIRE_HAT: VampireClothingItem = Registry.register(Registries.ITEM, id("vampire_hat"), VampireClothingItem(VampireClothingMaterial, ArmorItem.Type.HELMET, Item.Settings().maxCount(1).rarity(Rarity.UNCOMMON)))
    val VAMPIRE_COAT: VampireClothingItem = Registry.register(Registries.ITEM, id("vampire_coat"), VampireClothingItem(VampireClothingMaterial, ArmorItem.Type.CHESTPLATE, Item.Settings().maxCount(1).rarity(Rarity.UNCOMMON)))
    val VAMPIRE_TROUSERS: VampireClothingItem = Registry.register(Registries.ITEM, id("vampire_trousers"), VampireClothingItem(VampireClothingMaterial, ArmorItem.Type.LEGGINGS, Item.Settings().maxCount(1).rarity(Rarity.UNCOMMON)))
    val VAMPIRE_SHOES: VampireClothingItem = Registry.register(Registries.ITEM, id("vampire_shoes"), VampireClothingItem(VampireClothingMaterial, ArmorItem.Type.BOOTS, Item.Settings().maxCount(1).rarity(Rarity.UNCOMMON)))

    override fun onInitialize() {
    }
}