package com.williambl.haema.ritual;

import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.module.AraeModule;
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
        AraeModule.RESOURCE_KEY.location(); // force load
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
}
