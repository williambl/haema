package com.williambl.haema.ritual;

import com.mojang.serialization.Codec;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.module.AraeModule;
import com.williambl.haema.api.ritual.ritual.RitualAction;
import com.williambl.haema.api.ritual.ritual.RitualTrigger;
import com.williambl.haema.ritual.altar.RitualAltarBlock;
import com.williambl.haema.ritual.altar.RitualAltarBlockEntity;
import com.williambl.haema.ritual.module.ParticlesToCentreAraeModule;
import com.williambl.haema.ritual.ritual.RightClickRitualTrigger;
import com.williambl.haema.ritual.ritual.SpawnEntityRitualAction;
import net.fabricmc.fabric.api.object.builder.v1.block.entity.FabricBlockEntityTypeBuilder;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour;

import static com.williambl.haema.Haema.id;

public class HaemaRituals {
    public static void init() {
        RitualBlocks.init();
        RitualBlockEntities.init();
        RitualAraeModules.init();
        RitualActions.init();
        RitualTriggers.init();
    }

    public static class RitualBlocks {
        public static RitualAltarBlock RITUAL_ALTAR = HaemaUtil.registerWithItem(id("ritual_altar"), new RitualAltarBlock(BlockBehaviour.Properties.copy(Blocks.POLISHED_BLACKSTONE_BRICK_SLAB)), new Item.Properties());

        public static void init() {}
    }

    public static class RitualBlockEntities {
        public static BlockEntityType<RitualAltarBlockEntity> RITUAL_ALTAR = Registry.register(BuiltInRegistries.BLOCK_ENTITY_TYPE, id("ritual_altar"), FabricBlockEntityTypeBuilder.create(RitualAltarBlockEntity::new, RitualBlocks.RITUAL_ALTAR).build(null));

        public static void init() {
            RitualAltarBlockEntity.initReloadListener();
        }
    }

    public static class RitualAraeModules {
        public static Codec<? extends AraeModule> PARTICLES_TO_CENTRE = Registry.register(AraeModule.REGISTRY, id("particles_to_centre"), ParticlesToCentreAraeModule.CODEC.codec());

        public static void init() {}
    }

    public static class RitualActions {
        public static final Codec<SpawnEntityRitualAction> SPAWN_ENTITY = Registry.register(RitualAction.REGISTRY, id("spawn_entity"), SpawnEntityRitualAction.CODEC.codec());

        public static void init() {}
    }

    public static class RitualTriggers {
        public static final Codec<RightClickRitualTrigger> RIGHT_CLICK = Registry.register(RitualTrigger.REGISTRY, id("right_click"), RightClickRitualTrigger.CODEC.codec());

        public static void init() {}
    }
}
