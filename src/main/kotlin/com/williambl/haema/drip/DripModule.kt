package com.williambl.haema.drip

import com.williambl.haema.Haema
import com.williambl.haema.id
import net.fabricmc.api.ModInitializer
import net.minecraft.entity.EquipmentSlot
import net.minecraft.item.Item
import net.minecraft.util.registry.Registry

object DripModule: ModInitializer {

    val VAMPIRE_HAT = Registry.register(Registry.ITEM, id("vampire_hat"), VampireClothingItem(VampireClothingMaterial, EquipmentSlot.HEAD, Item.Settings().group(Haema.ITEM_GROUP).maxCount(1)))
    val VAMPIRE_COAT = Registry.register(Registry.ITEM, id("vampire_coat"), VampireClothingItem(VampireClothingMaterial, EquipmentSlot.CHEST, Item.Settings().group(Haema.ITEM_GROUP).maxCount(1)))
    val VAMPIRE_TROUSERS = Registry.register(Registry.ITEM, id("vampire_trousers"), VampireClothingItem(VampireClothingMaterial, EquipmentSlot.LEGS, Item.Settings().group(Haema.ITEM_GROUP).maxCount(1)))
    val VAMPIRE_SHOES = Registry.register(Registry.ITEM, id("vampire_shoes"), VampireClothingItem(VampireClothingMaterial, EquipmentSlot.FEET, Item.Settings().group(Haema.ITEM_GROUP).maxCount(1)))

    override fun onInitialize() {
    }
}