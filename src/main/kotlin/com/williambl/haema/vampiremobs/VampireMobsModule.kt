package com.williambl.haema.vampiremobs

import com.mojang.serialization.Codec
import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.component.dash.AiControlledEntityDashAbilityComponent
import com.williambl.haema.ability.component.dash.DashAbilityComponent
import com.williambl.haema.ability.component.strength.EntityStrengthAbilityComponent
import com.williambl.haema.ability.component.strength.StrengthAbilityComponent
import com.williambl.haema.api.BloodDrinkingEvents
import com.williambl.haema.component.EntityVampireComponent
import com.williambl.haema.component.VampireComponent
import com.williambl.haema.id
import com.williambl.haema.vampiremobs.elder.ElderVampireEntity
import com.williambl.haema.vampiremobs.elder.SunShieldBlock
import com.williambl.haema.vampiremobs.elder.SunShieldProjectileEntity
import dev.onyxstudios.cca.api.v3.entity.EntityComponentFactoryRegistry
import dev.onyxstudios.cca.api.v3.entity.EntityComponentInitializer
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.biome.v1.BiomeModifications
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricDefaultAttributeRegistry
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricEntityTypeBuilder
import net.minecraft.block.AbstractBlock
import net.minecraft.block.MapColor
import net.minecraft.block.Material
import net.minecraft.entity.*
import net.minecraft.entity.ai.brain.MemoryModuleType
import net.minecraft.entity.mob.HostileEntity
import net.minecraft.entity.mob.ZombieEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.registry.Registries
import net.minecraft.registry.Registry
import net.minecraft.registry.RegistryKeys
import net.minecraft.registry.tag.TagKey

import net.minecraft.world.Heightmap
import net.minecraft.world.World
import net.minecraft.world.biome.Biome
import java.util.*

object VampireMobsModule: ModInitializer, EntityComponentInitializer {
    val VAMPIRIC_ZOMBIE: EntityType<VampiricZombieEntity> = Registry.register(
        Registries.ENTITY_TYPE,
        id("vampiric_zombie"),
        FabricEntityTypeBuilder.createMob<VampiricZombieEntity>()
            .spawnGroup(SpawnGroup.MONSTER)
            .entityFactory(::VampiricZombieEntity)
            .dimensions(EntityDimensions.fixed(0.6f, 1.95f))
            .spawnRestriction(SpawnRestriction.Location.ON_GROUND, Heightmap.Type.MOTION_BLOCKING_NO_LEAVES, HostileEntity::canSpawnInDark)
            .build()
    )

    val VAMPIRAGER: EntityType<VampiragerEntity> = Registry.register(
        Registries.ENTITY_TYPE,
        id("vampirager"),
        FabricEntityTypeBuilder.createMob<VampiragerEntity>()
            .spawnGroup(SpawnGroup.MONSTER)
            .entityFactory(::VampiragerEntity)
            .dimensions(EntityDimensions.fixed(0.6f, 1.95f))
            .spawnRestriction(SpawnRestriction.Location.ON_GROUND, Heightmap.Type.MOTION_BLOCKING_NO_LEAVES, HostileEntity::canSpawnInDark)
            .build()
    )

    val ELDER_VAMPIRE: EntityType<ElderVampireEntity> = Registry.register(
        Registries.ENTITY_TYPE,
        id("elder_vampire"),
        FabricEntityTypeBuilder.createMob<ElderVampireEntity>()
            .spawnGroup(SpawnGroup.MONSTER)
            .entityFactory(::ElderVampireEntity)
            .dimensions(EntityDimensions.fixed(0.6f, 1.95f))
            .spawnRestriction(SpawnRestriction.Location.ON_GROUND, Heightmap.Type.MOTION_BLOCKING_NO_LEAVES, HostileEntity::canSpawnInDark)
            .build()
    )

    val SUN_SHIELD_PROJECTILE: EntityType<SunShieldProjectileEntity> = Registry.register(
        Registry.ENTITY_TYPE,
        id("sun_shield_projectile"),
        FabricEntityTypeBuilder.create<SunShieldProjectileEntity>(SpawnGroup.MISC)
            .entityFactory(::SunShieldProjectileEntity)
            .dimensions(EntityDimensions.fixed(0.3125F, 0.3125F))
            .trackRangeChunks(4)
            .build()
    )

    val SUN_SHIELD_BLOCK: SunShieldBlock = Registry.register(
        Registry.BLOCK,
        id("sun_shield"),
        SunShieldBlock(AbstractBlock.Settings.of(Material.Builder(MapColor.CLEAR).replaceable().allowsMovement().build()).breakInstantly().ticksRandomly())
    )

    val ATTACK_TARGETS_MEMORY: MemoryModuleType<List<LivingEntity>> = Registry.register(
        Registry.MEMORY_MODULE_TYPE,
        id("attack_targets"),
        MemoryModuleType(Optional.empty())
    )

    val ABILITIES_COOLDOWN_MEMORY: MemoryModuleType<Int> = Registry.register(
        Registry.MEMORY_MODULE_TYPE,
        id("abilities_cooldown"),
        MemoryModuleType(Optional.of(Codec.INT))
    )

    val BLOOD_DRAIN_COOLDOWN_MEMORY: MemoryModuleType<Int> = Registry.register(
        Registry.MEMORY_MODULE_TYPE,
        id("blood_drain_cooldown"),
        MemoryModuleType(Optional.of(Codec.INT))
    )

    val SUN_SHIELD_COOLDOWN_MEMORY: MemoryModuleType<Int> = Registry.register(
        Registry.MEMORY_MODULE_TYPE,
        id("sun_shield_cooldown"),
        MemoryModuleType(Optional.of(Codec.INT))
    )

    val DASH_COOLDOWN_MEMORY: MemoryModuleType<Int> = Registry.register(
        Registry.MEMORY_MODULE_TYPE,
        id("dash_cooldown"),
        MemoryModuleType(Optional.of(Codec.INT))
    )

    private val BIOME_SPAWNS_VAMPIRAGERS: TagKey<Biome> = TagKey.of(RegistryKeys.BIOME, id("spawns_vampiragers"))

    override fun onInitialize() {
        BloodDrinkingEvents.ON_BLOOD_DRINK.register { drinker, target, world ->
            if (drinker !is PlayerEntity && target is ZombieEntity && target !is VampiricZombieEntity) {
                VampiricZombieEntity.convert(target)
            }
        }

        FabricDefaultAttributeRegistry.register(VAMPIRIC_ZOMBIE, ZombieEntity.createZombieAttributes())
        FabricDefaultAttributeRegistry.register(VAMPIRAGER, VampiragerEntity.createVampiragerAttributes())
        FabricDefaultAttributeRegistry.register(ELDER_VAMPIRE, VampiragerEntity.createVampiragerAttributes())

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

        registry.registerFor(ElderVampireEntity::class.java, VampireComponent.entityKey) { entity -> EntityVampireComponent(
            entity,
            isVampireInitial = true,
            isPermanentVampireInitial = true,
            absoluteBloodInitial = 20.0,
            abilitiesInitial = mutableMapOf(
                AbilityModule.STRENGTH to 1,
                AbilityModule.DASH to 1,
                AbilityModule.INVISIBILITY to 0,
                AbilityModule.IMMORTALITY to 0,
                AbilityModule.VISION to 1,
                AbilityModule.MIST_FORM to 0
            )
        )}

        registry.registerFor(ElderVampireEntity::class.java, StrengthAbilityComponent.entityKey, ::EntityStrengthAbilityComponent)

        registry.registerFor(ElderVampireEntity::class.java, DashAbilityComponent.entityKey) { entity -> AiControlledEntityDashAbilityComponent(
            entity,
            entity::dashTarget
        )}
    }
}