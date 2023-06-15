package com.williambl.haema.ritual.ritual;

import com.google.gson.JsonObject;
import com.mojang.datafixers.util.Pair;
import com.mojang.serialization.Codec;
import com.mojang.serialization.JsonOps;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContext;
import com.williambl.dfunc.api.context.DFContextSpec;
import com.williambl.haema.Haema;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.ritual.HaemaRituals;
import net.minecraft.core.HolderSet;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.RegistryCodecs;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import xyz.nucleoid.codecs.MoreCodecs;

import java.util.List;
import java.util.function.Function;

public record RitualRecipe(
        ResourceLocation id,
        Fluid fluid,
        List<Ingredient> ingredients,
        HolderSet<RitualArae> acceptableAraes,
        DFunction<Boolean> canPlayerUse,
        List<RitualAction> actions) implements Recipe<RitualContainer> {
    public RitualRecipe(Partial partial, ResourceLocation id) {
        this(id, partial.fluid(), partial.ingredients(), partial.acceptableAraes(), partial.canPlayerUse(), partial.actions());
    }

    @Override
    public boolean matches(RitualContainer container, Level level) {
        return this.canPlayerUse.apply(DFContext.entity(container.player()))
                && this.isAraeAcceptable(container.arae(), level.registryAccess().registryOrThrow(RitualArae.REGISTRY_KEY))
                && HaemaUtil.allMatchOne(container.items, this.ingredients)
                && this.fluid.isSame(container.fluid());
    }

    private boolean isAraeAcceptable(RitualArae toCheck, Registry<RitualArae> registry) {
        return this.acceptableAraes.contains(registry.wrapAsHolder(toCheck));
    }

    @Override
    public ItemStack assemble(RitualContainer container, RegistryAccess registryAccess) {
        if (!this.matches(container, container.player().getLevel())) {
            return ItemStack.EMPTY;
        }

        return ItemStack.EMPTY;
    }

    @Override
    public boolean canCraftInDimensions(int i, int j) {
        return true;
    }

    @Override
    public ItemStack getResultItem(RegistryAccess registryAccess) {
        return ItemStack.EMPTY;
    }

    @Override
    public ResourceLocation getId() {
        return this.id;
    }

    @Override
    public RecipeSerializer<?> getSerializer() {
        return HaemaRituals.RitualRecipeSerializers.RITUAL;
    }

    @Override
    public RecipeType<?> getType() {
        return HaemaRituals.RitualRecipeTypes.RITUAL;
    }

    private record Partial(
            Fluid fluid,
            List<Ingredient> ingredients,
            HolderSet<RitualArae> acceptableAraes,
            DFunction<Boolean> canPlayerUse,
            List<RitualAction> actions
    ) {
        private static final Codec<Partial> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                BuiltInRegistries.FLUID.byNameCodec().fieldOf("fluid").forGetter(Partial::fluid),
                MoreCodecs.INGREDIENT.listOf().fieldOf("ingredients").forGetter(Partial::ingredients),
                RegistryCodecs.homogeneousList(RitualArae.REGISTRY_KEY).fieldOf("acceptable_araes").forGetter(Partial::acceptableAraes),
                DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY), Function.identity()).fieldOf("can_player_use").forGetter(Partial::canPlayerUse),
                RitualAction.ACTION_CODEC.listOf().fieldOf("actions").forGetter(Partial::actions)
        ).apply(instance, Partial::new));
    }

    public static class Serializer implements RecipeSerializer<RitualRecipe> {
        @Override
        public RitualRecipe fromJson(ResourceLocation id, JsonObject jsonObject) {
            var partial = Partial.CODEC.decode(JsonOps.INSTANCE, jsonObject)
                    .map(Pair::getFirst)
                    .getOrThrow(false, e -> Haema.LOGGER.error("Error deserialising ritual {}: {}", id, e));
            return new RitualRecipe(partial, id);
        }

        @Override
        public RitualRecipe fromNetwork(ResourceLocation resourceLocation, FriendlyByteBuf friendlyByteBuf) {
            //TODO
            return null;
        }

        @Override
        public void toNetwork(FriendlyByteBuf friendlyByteBuf, RitualRecipe recipe) {
            friendlyByteBuf.writeVarInt(BuiltInRegistries.FLUID.getId(recipe.fluid()));
            friendlyByteBuf.writeVarInt(recipe.ingredients().size());
            for (var ingredient : recipe.ingredients()) {
                ingredient.toNetwork(friendlyByteBuf);
            }
            //TODO finish
        }
    }
}
