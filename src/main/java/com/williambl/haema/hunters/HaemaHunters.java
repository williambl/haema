package com.williambl.haema.hunters;

import net.fabricmc.fabric.api.object.builder.v1.entity.FabricDefaultAttributeRegistry;
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricEntityTypeBuilder;
import net.fabricmc.fabric.impl.object.builder.FabricEntityType;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.EntityDimensions;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.ai.attributes.Attributes;

import static com.williambl.haema.Haema.id;

public class HaemaHunters {
    public static void init() {
        HunterEntityTypes.init();
    }

    public static class HunterEntityTypes {
        public static final EntityType<VampireHunter> VAMPIRE_HUNTER = Registry.register(BuiltInRegistries.ENTITY_TYPE, id("vampire_hunter"), FabricEntityTypeBuilder.createMob().spawnGroup(MobCategory.CREATURE).entityFactory(VampireHunter::new).dimensions(EntityDimensions.fixed(0.6f, 1.95f)).trackRangeBlocks(128).trackedUpdateRate(3).spawnableFarFromPlayer().build());

        public static void init() {
            FabricDefaultAttributeRegistry.register(VAMPIRE_HUNTER, VampireHunter.createHunterAttributes());
        }
    }
}
