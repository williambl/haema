package com.williambl.haema.blood

import com.williambl.haema.blood.injector.registerInjectors
import com.williambl.haema.util.addTradesToProfession
import net.fabricmc.fabric.api.loot.v1.FabricLootPoolBuilder
import net.fabricmc.fabric.api.loot.v1.FabricLootSupplierBuilder
import net.fabricmc.fabric.api.loot.v1.event.LootTableLoadingCallback
import net.minecraft.client.item.TooltipContext
import net.minecraft.item.Item
import net.minecraft.item.ItemGroup
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.loot.LootManager
import net.minecraft.loot.entry.ItemEntry
import net.minecraft.loot.provider.number.ConstantLootNumberProvider
import net.minecraft.resource.ResourceManager
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry
import net.minecraft.village.TradeOffer
import net.minecraft.village.TradeOffers
import net.minecraft.village.VillagerProfession
import net.minecraft.world.World

fun registerBlood() {
    Registry.register(
        Registry.ITEM,
        Identifier("haema:vampire_blood"),
        object : Item(Settings().group(ItemGroup.MISC)) {
            override fun appendTooltip(stack: ItemStack?, world: World?, tooltip: MutableList<Text>, context: TooltipContext?) {
                super.appendTooltip(stack, world, tooltip, context)
                tooltip.add(TranslatableText("item.haema.vampire_blood.desc").formatted(Formatting.DARK_RED))
            }
        }
    ).also {
        addTradesToProfession(VillagerProfession.CLERIC, 3, TradeOffers.Factory { _, _ ->
            TradeOffer(ItemStack(Items.EMERALD, 5), ItemStack(it), 1, 30, 0.05f)
        })
    }

    val dungeonLootTable = Identifier("minecraft:chests/simple_dungeon")
    val jungleTempleLootTable = Identifier("minecraft:chests/jungle_temple")
    val desertPyramidLootTable = Identifier("minecraft:chests/desert_pyramid")
    LootTableLoadingCallback.EVENT.register(LootTableLoadingCallback { resourceManager: ResourceManager?, lootManager: LootManager?, id: Identifier?, supplier: FabricLootSupplierBuilder, setter: LootTableLoadingCallback.LootTableSetter? ->
        if (id == dungeonLootTable || id == jungleTempleLootTable || id == desertPyramidLootTable) {
            val poolBuilder: FabricLootPoolBuilder = FabricLootPoolBuilder.builder()
                .rolls(ConstantLootNumberProvider.create(1f))
                .withEntry(
                    ItemEntry.builder(Registry.ITEM.get(Identifier("haema:vampire_blood")))
                        .weight(if (id == dungeonLootTable) 10 else 5)
                        .build()
                )
                .withEntry(
                    ItemEntry.builder(Items.AIR)
                        .weight(10)
                        .build()
                )
            supplier.withPool(poolBuilder.build())
        }
    })

    registerInjectors()
}