package com.williambl.haema.spells

import com.williambl.haema.id
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder
import net.fabricmc.fabric.api.event.registry.RegistryAttribute
import net.minecraft.item.Item
import net.minecraft.item.ItemGroup
import net.minecraft.util.registry.Registry

object SpellsModule: ModInitializer {
    val SPELL_REGISTRY: Registry<Spell> = FabricRegistryBuilder.createSimple(Spell::class.java, id("spell")).attribute(RegistryAttribute.SYNCED).buildAndRegister()

    val DISSOLUTION: Spell = Registry.register(SPELL_REGISTRY, id("dissolution"), Spell())

    val SPELL_SCROLL: SpellScrollItem = Registry.register(
        Registry.ITEM,
        id("spell_scroll"),
        SpellScrollItem(DISSOLUTION, Item.Settings().group(ItemGroup.MISC))
    )

    override fun onInitialize() {
    }
}