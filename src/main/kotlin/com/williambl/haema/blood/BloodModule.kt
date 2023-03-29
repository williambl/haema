package com.williambl.haema.blood

import com.williambl.haema.id
import com.williambl.haema.util.addTradesToProfession
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.loot.v2.FabricLootTableBuilder
import net.fabricmc.fabric.api.loot.v2.LootTableEvents
import net.fabricmc.fabric.api.loot.v2.LootTableSource
import net.minecraft.block.DispenserBlock
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.loot.LootManager
import net.minecraft.loot.LootPool
import net.minecraft.loot.entry.LootTableEntry
import net.minecraft.loot.provider.number.ConstantLootNumberProvider
import net.minecraft.registry.Registries
import net.minecraft.registry.Registry
import net.minecraft.resource.ResourceManager
import net.minecraft.util.Identifier
import net.minecraft.village.TradeOffer
import net.minecraft.village.TradeOffers
import net.minecraft.village.VillagerProfession

object BloodModule: ModInitializer {
    val VAMPIRE_BLOOD: VampireBloodItem = Registry.register(
        Registries.ITEM,
        id("vampire_blood"),
        VampireBloodItem()
    )

    val VAMPIRE_BLOOD_INJECTOR: VampireBloodInjectorItem = Registry.register(
        Registries.ITEM,
        id("vampire_blood_injector"),
        VampireBloodInjectorItem(Item.Settings().maxCount(1))
    )

    val EMPTY_VAMPIRE_BLOOD_INJECTOR: EmptyVampireBloodInjectorItem = Registry.register(
        Registries.ITEM,
        id("empty_vampire_blood_injector"),
        EmptyVampireBloodInjectorItem(Item.Settings().maxCount(1))
    )

    override fun onInitialize() {
        addTradesToProfession(VillagerProfession.CLERIC, 3, TradeOffers.Factory { _, _ ->
            TradeOffer(ItemStack(Items.EMERALD, 5), ItemStack(VAMPIRE_BLOOD), 1, 30, 0.05f)
        })

        DispenserBlock.registerBehavior(VAMPIRE_BLOOD_INJECTOR, VampireBloodInjectorItem.DispenserBehavior)
        DispenserBlock.registerBehavior(EMPTY_VAMPIRE_BLOOD_INJECTOR, EmptyVampireBloodInjectorItem.DispenserBehavior)

        val dungeonLootTable = Identifier("minecraft:chests/simple_dungeon")
        val jungleTempleLootTable = Identifier("minecraft:chests/jungle_temple")
        val desertPyramidLootTable = Identifier("minecraft:chests/desert_pyramid")
        LootTableEvents.MODIFY.register { resourceManager: ResourceManager?, lootManager: LootManager?, id: Identifier?, tableBuilder: FabricLootTableBuilder, source: LootTableSource ->
            if (id == dungeonLootTable || id == jungleTempleLootTable || id == desertPyramidLootTable) {
                val poolBuilder: LootPool.Builder = LootPool.builder()
                    .rolls(ConstantLootNumberProvider.create(1f))
                    .with(
                        LootTableEntry.builder(id("injected/chests/${if (id == dungeonLootTable) "dungeon" else "temple"}_blood"))
                            .weight(12)
                            .build()
                    )
                tableBuilder.pool(poolBuilder.build())
            }
        }
    }
}
