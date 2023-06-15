package com.williambl.haema.ritual;

import com.mojang.serialization.Codec;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.module.AraeModule;
import com.williambl.haema.ritual.altar.RitualAltarBlock;
import com.williambl.haema.ritual.altar.RitualAltarBlockEntity;
import com.williambl.haema.ritual.module.ParticlesToCentreAraeModule;
import com.williambl.haema.ritual.ritual.RitualRecipe;
import net.fabricmc.fabric.api.object.builder.v1.block.entity.FabricBlockEntityTypeBuilder;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockBehaviour;

import static com.williambl.haema.Haema.id;

public class HaemaRituals {
    public static void init() {
        RitualBlocks.init();
        RitualBlockEntities.init();
        RitualAraeModules.init();
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

    public static class RitualRecipeTypes {
        public static RecipeType<RitualRecipe> RITUAL = RecipeType.register(id("ritual").toString());

        public static void init() {}
    }

    public static class RitualRecipeSerializers {
        public static RecipeSerializer<RitualRecipe> RITUAL = Registry.register(BuiltInRegistries.RECIPE_SERIALIZER, id("ritual"), new RitualRecipe.Serializer());

        public static void init() {}
    }
}
