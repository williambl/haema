package com.williambl.haema.vampire_mobs;

import com.williambl.haema.api.vampire.VampirismSource;
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricEntityTypeBuilder;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.*;
import net.minecraft.world.entity.ai.memory.MemoryModuleType;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.level.levelgen.Heightmap;

import java.util.Optional;

import static com.williambl.haema.Haema.id;

public class HaemaVampireMobs {
    public static void init() {
        VampireMobEntityTypes.init();
        VampireMobMemoryModuleTypes.init();
        VampireMobMemoryModuleTypes.init();
    }

    public static class VampireMobEntityTypes {
        public static final EntityType<Vampirager> VAMPIRAGER = Registry.register(BuiltInRegistries.ENTITY_TYPE, id("vampirager"), FabricEntityTypeBuilder.createMob().spawnGroup(MobCategory.MONSTER).entityFactory(Vampirager::new).dimensions(EntityDimensions.fixed(0.6f, 1.95f)).spawnRestriction(SpawnPlacements.Type.ON_GROUND, Heightmap.Types.MOTION_BLOCKING_NO_LEAVES, Monster::checkMonsterSpawnRules).build());

        private static void init() {}
    }

    public static class VampireMobVampirismSources {
        public static final ResourceKey<VampirismSource> VAMPIRAGER_SPAWN = ResourceKey.create(VampirismSource.REGISTRY_KEY, id("vampirager_spawn"));

        private static void init() {}
    }

    public static class VampireMobMemoryModuleTypes {
        public static final MemoryModuleType<LivingEntity> BLOOD_DRINKING_TARGET = Registry.register(BuiltInRegistries.MEMORY_MODULE_TYPE, id("blood_drinking_target"), new MemoryModuleType<>(Optional.empty()));

        private static void init() {}
    }
}
