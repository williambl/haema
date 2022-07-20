package com.williambl.haema.hunter

import com.williambl.haema.Haema
import com.williambl.haema.hunter.structure.SmallVampireHunterOutpostStructure
import com.williambl.haema.id
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerTickEvents
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricDefaultAttributeRegistry
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricEntityTypeBuilder
import net.minecraft.entity.EntityDimensions
import net.minecraft.entity.EntityType
import net.minecraft.entity.SpawnGroup
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.mob.HostileEntity
import net.minecraft.item.Item
import net.minecraft.util.registry.Registry
import net.minecraft.world.Difficulty
import net.minecraft.world.gen.structure.StructureType

object VampireHunterModule: ModInitializer {
    val VAMPIRE_HUNTER: EntityType<VampireHunterEntity> =
        Registry.register(
            Registry.ENTITY_TYPE,
            id("vampire_hunter"),
            FabricEntityTypeBuilder.create<VampireHunterEntity>(SpawnGroup.CREATURE) { type, world -> VampireHunterEntity(type, world) }
                .dimensions(EntityDimensions.fixed(0.6f, 1.95f))
                .trackRangeBlocks(128).trackedUpdateRate(3).spawnableFarFromPlayer().build()
        )

    val VAMPIRE_HUNTER_CONTRACT: VampireHunterContract = Registry.register(
        Registry.ITEM,
        id("vampire_hunter_contract"),
        VampireHunterContract(Item.Settings().group(Haema.ITEM_GROUP))
    )

    val SMALL_VAMPIRE_HUNTER_OUTPOST_FEATURE: StructureType<SmallVampireHunterOutpostStructure> = Registry.register(
        Registry.STRUCTURE_TYPE,
        id("small_vampire_hunter_outpost"),
        StructureType { SmallVampireHunterOutpostStructure.CODEC }
    )

    override fun onInitialize() {
        FabricDefaultAttributeRegistry.register(
            VAMPIRE_HUNTER,
            HostileEntity.createHostileAttributes().add(EntityAttributes.GENERIC_MOVEMENT_SPEED, 0.35)
                .add(EntityAttributes.GENERIC_MAX_HEALTH, 20.0)
                .add(EntityAttributes.GENERIC_ATTACK_DAMAGE, 5.0)
                .add(EntityAttributes.GENERIC_FOLLOW_RANGE, 64.0)
        )

        val spawner = VampireHunterSpawner()
        ServerTickEvents.END_SERVER_TICK.register { server ->
            server.worlds.forEach { world ->
                spawner.spawn(
                    world,
                    server.saveProperties.difficulty != Difficulty.PEACEFUL,
                    server.shouldSpawnAnimals()
                )
            }
        }
    }
}