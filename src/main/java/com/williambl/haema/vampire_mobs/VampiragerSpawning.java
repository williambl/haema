package com.williambl.haema.vampire_mobs;

import net.fabricmc.fabric.api.biome.v1.BiomeModifications;
import net.fabricmc.fabric.api.biome.v1.BiomeSelectors;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;

public class VampiragerSpawning {
    public static void init() {
        BiomeModifications.addSpawn(BiomeSelectors.spawnsOneOf(EntityType.WITCH)
                .and(BiomeSelectors.tag(HaemaVampireMobs.VampireMobTags.WITHOUT_VAMPIRAGERS).negate()),
                MobCategory.MONSTER,
                HaemaVampireMobs.VampireMobEntityTypes.VAMPIRAGER,
                5,
                1,
                1);
    }
}
