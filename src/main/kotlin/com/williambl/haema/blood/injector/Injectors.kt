package com.williambl.haema.blood.injector

import net.minecraft.block.DispenserBlock
import net.minecraft.item.Item
import net.minecraft.item.ItemGroup
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry

fun registerInjectors() {
    Registry.register(
        Registry.ITEM,
        Identifier("haema:vampire_blood_injector"),
        VampireBloodInjectorItem(Item.Settings().group(ItemGroup.TOOLS).maxCount(1))
    ).also {
        DispenserBlock.registerBehavior(it, VampireBloodInjectorItem.DispenserBehavior)
    }

    Registry.register(
        Registry.ITEM,
        Identifier("haema:empty_vampire_blood_injector"),
        EmptyVampireBloodInjectorItem(Item.Settings().group(ItemGroup.TOOLS).maxCount(1))
    ).also {
        DispenserBlock.registerBehavior(it, EmptyVampireBloodInjectorItem.DispenserBehavior)
    }
}