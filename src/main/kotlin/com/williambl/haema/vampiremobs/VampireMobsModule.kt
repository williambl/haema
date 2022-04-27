package com.williambl.haema.vampiremobs

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.component.EntityVampireComponent
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.id
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricDefaultAttributeRegistry
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricEntityTypeBuilder
import net.minecraft.entity.EntityDimensions
import net.minecraft.entity.SpawnGroup
import net.minecraft.entity.mob.ZombieEntity
import net.minecraft.util.registry.Registry

object VampireMobsModule: ModInitializer, EntityComponentInitializer {
    val VAMPIRIC_ZOMBIE = Registry.register(
        Registry.ENTITY_TYPE,
        id("vampiric_zombie"),
        FabricEntityTypeBuilder.create(SpawnGroup.MONSTER, ::VampiricZombieEntity).dimensions(EntityDimensions.fixed(0.6f, 1.95f)).build()
    )

    override fun onInitialize() {
        BloodDrinkingEvents.ON_BLOOD_DRINK.register { drinker, target, world ->
            if (target is ZombieEntity && target !is VampiricZombieEntity) {
                VampiricZombieEntity.convert(target)
            }
        }
    }

    override fun registerEntityComponentFactories(registry: EntityComponentFactoryRegistry) {
        registry.registerFor(VampiricZombieEntity::class.java, VampireComponent.entityKey) { entity -> EntityVampireComponent(
            entity,
            isVampireInitial = true,
            isPermanentVampireInitial = true,
            abilitiesInitial = mutableMapOf(
                AbilityModule.STRENGTH to 1,
                AbilityModule.DASH to 1,
                AbilityModule.INVISIBILITY to 0,
                AbilityModule.IMMORTALITY to 0,
                AbilityModule.VISION to 1,
                AbilityModule.MIST_FORM to 0
            )
        )}

        FabricDefaultAttributeRegistry.register(VAMPIRIC_ZOMBIE, ZombieEntity.createZombieAttributes())
    }
}