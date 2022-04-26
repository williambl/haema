package com.williambl.haema.spells

import com.williambl.haema.Haema
import com.williambl.haema.id
import com.williambl.haema.spells.component.EntitySpellsComponent
import com.williambl.haema.spells.component.SpellsComponent
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer
import dev.onyxstudios.cca.api.v3.entity.RespawnCopyStrategy
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.event.registry.FabricRegistryBuilder
import net.fabricmc.fabric.api.event.registry.RegistryAttribute
import net.minecraft.item.Item
import net.minecraft.util.registry.Registry

object SpellsModule: ModInitializer, EntityComponentInitializer {
    val SPELL_REGISTRY: Registry<Spell> = FabricRegistryBuilder.createSimple(Spell::class.java, id("spell")).attribute(RegistryAttribute.SYNCED).buildAndRegister()

    val DISSOLUTION: Spell = Registry.register(SPELL_REGISTRY, id("dissolution"), DissolutionSpell())

    val SPELL_SCROLL: SpellScrollItem = Registry.register(
        Registry.ITEM,
        id("spell_scroll"),
        SpellScrollItem(DISSOLUTION, Item.Settings().group(Haema.ITEM_GROUP))
    )

    override fun onInitialize() {
    }

    override fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
        registry.registerForPlayers(SpellsComponent.entityKey, ::EntitySpellsComponent, RespawnCopyStrategy.LOSSLESS_ONLY)
    }
}