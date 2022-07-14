package com.williambl.haema.vampiremobs

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.component.dash.AiControlledEntityDashAbilityComponent
import com.williambl.haema.ability.component.dash.DashAbilityComponent
import com.williambl.haema.ability.component.strength.EntityStrengthAbilityComponent
import com.williambl.haema.ability.component.strength.StrengthAbilityComponent
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.component.EntityVampireComponent
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.id
import com.williambl.haema.vampiremobs.elder_vampire.VampiricEssenceBlock
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.biome.v1.BiomeModifications
import net.fabricmc.fabric.api.`object`.builder.v1.block.FabricMaterialBuilder
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricDefaultAttributeRegistry
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricEntityTypeBuilder
import net.minecraft.block.AbstractBlock
import net.minecraft.block.BlockState
import net.minecraft.block.MapColor
import net.minecraft.entity.*
import net.minecraft.entity.mob.HostileEntity
import net.minecraft.entity.mob.ZombieEntity
import net.minecraft.tag.TagKey
import net.minecraft.util.math.BlockPos
import net.minecraft.util.registry.Registry
import net.minecraft.world.BlockView
import net.minecraft.world.Heightmap
import net.minecraft.world.World
import net.minecraft.world.biome.Biome

object VampireMobsModule: ModInitializer, EntityComponentInitializer {
    val VAMPIRIC_ZOMBIE: EntityType<VampiricZombieEntity> = Registry.register(
        Registry.ENTITY_TYPE,
        id("vampiric_zombie"),
        FabricEntityTypeBuilder.createMob<VampiricZombieEntity>()
            .spawnGroup(SpawnGroup.MONSTER)
            .entityFactory(::VampiricZombieEntity)
            .dimensions(EntityDimensions.fixed(0.6f, 1.95f))
            .spawnRestriction(SpawnRestriction.Location.ON_GROUND, Heightmap.Type.MOTION_BLOCKING_NO_LEAVES, HostileEntity::canSpawnInDark)
            .build()
    )

    val VAMPIRAGER: EntityType<VampiragerEntity> = Registry.register(
        Registry.ENTITY_TYPE,
        id("vampirager"),
        FabricEntityTypeBuilder.createMob<VampiragerEntity>()
            .spawnGroup(SpawnGroup.MONSTER)
            .entityFactory(::VampiragerEntity)
            .dimensions(EntityDimensions.fixed(0.6f, 1.95f))
            .spawnRestriction(SpawnRestriction.Location.ON_GROUND, Heightmap.Type.MOTION_BLOCKING_NO_LEAVES, HostileEntity::canSpawnInDark)
            .build()
    )

    val VAMPIRIC_ESSENCE: VampiricEssenceBlock = Registry.register(
        Registry.BLOCK,
        id("vampiric_essence"),
        VampiricEssenceBlock(AbstractBlock.Settings.of(FabricMaterialBuilder(MapColor.BLACK).destroyedByPiston().build())
            .nonOpaque()
            .allowsSpawning {_, _, _, _ -> false}
            .solidBlock(::never)
            .suffocates(::never)
            .blockVision(::never)
        )
    )

    private val BIOME_SPAWNS_VAMPIRAGERS: TagKey<Biome> = TagKey.of(Registry.BIOME_KEY, id("spawns_vampiragers"))

    override fun onInitialize() {
        BloodDrinkingEvents.ON_BLOOD_DRINK.register { drinker, target, world ->
            if (target is ZombieEntity && target !is VampiricZombieEntity) {
                VampiricZombieEntity.convert(target)
            }
        }

        FabricDefaultAttributeRegistry.register(VAMPIRIC_ZOMBIE, ZombieEntity.createZombieAttributes())
        FabricDefaultAttributeRegistry.register(VAMPIRAGER, VampiragerEntity.createVampiragerAttributes())

        BiomeModifications.addSpawn({ it.hasTag(BIOME_SPAWNS_VAMPIRAGERS) }, SpawnGroup.MONSTER, VAMPIRAGER, 4, 1, 1)

        BloodDrinkingEvents.ON_BLOOD_DRINK.register { drinker: LivingEntity, target: LivingEntity, world: World ->
            if (drinker is VampiricZombieEntity && target is ZombieEntity) {
                val converted = target.convertTo(VAMPIRIC_ZOMBIE, true)
                converted?.owner = drinker.owner
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
                AbilityModule.DASH to 0,
                AbilityModule.INVISIBILITY to 0,
                AbilityModule.IMMORTALITY to 0,
                AbilityModule.VISION to 1,
                AbilityModule.MIST_FORM to 0
            )
        )}

        registry.registerFor(VampiricZombieEntity::class.java, StrengthAbilityComponent.entityKey, ::EntityStrengthAbilityComponent)

        registry.registerFor(VampiragerEntity::class.java, VampireComponent.entityKey) { entity -> EntityVampireComponent(
            entity,
            isVampireInitial = true,
            isPermanentVampireInitial = true,
            absoluteBloodInitial = 20.0,
            abilitiesInitial = mutableMapOf(
                AbilityModule.STRENGTH to 1,
                AbilityModule.DASH to 0,
                AbilityModule.INVISIBILITY to 0,
                AbilityModule.IMMORTALITY to 0,
                AbilityModule.VISION to 1,
                AbilityModule.MIST_FORM to 0
            )
        )}

        registry.registerFor(VampiragerEntity::class.java, StrengthAbilityComponent.entityKey, ::EntityStrengthAbilityComponent)

        registry.registerFor(VampiragerEntity::class.java, DashAbilityComponent.entityKey) { entity -> AiControlledEntityDashAbilityComponent(
            entity,
            entity::dashTarget
        )}
    }

    private fun never(state: BlockState, world: BlockView, pos: BlockPos): Boolean {
        return false
    }
}