package com.williambl.haema.ritual.ritual;

import com.google.gson.JsonObject;
import com.mojang.datafixers.util.Either;
import com.mojang.datafixers.util.Pair;
import com.mojang.serialization.JsonOps;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContext;
import com.williambl.dfunc.api.context.DFContextSpec;
import com.williambl.haema.Haema;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.ritual.HaemaRituals;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.util.GsonHelper;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

public record RitualRecipe(
        ResourceLocation id,
        Fluid fluid,
        List<Ingredient> ingredients,
        Either<ResourceKey<RitualArae>, TagKey<RitualArae>> acceptableAraes,
        DFunction<Boolean> canPlayerUse,
        List<RitualAction> actions) implements Recipe<RitualContainer> {
    @Override
    public boolean matches(RitualContainer container, Level level) {
        return this.canPlayerUse.apply(DFContext.entity(container.player()))
                && this.isAraeAcceptable(container.arae(), level.registryAccess().registryOrThrow(RitualArae.REGISTRY_KEY))
                && HaemaUtil.allMatchOne(container.items, this.ingredients)
                && this.fluid.isSame(container.fluid());
    }

    private boolean isAraeAcceptable(RitualArae toCheck, Registry<RitualArae> registry) {
        return this.acceptableAraes.map(
                r -> registry.getResourceKey(toCheck).filter(r::equals).isPresent(),
                t -> registry.getOrCreateTag(t).contains(registry.wrapAsHolder(toCheck))
        );
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

    public static class Serializer implements RecipeSerializer<RitualRecipe> {
        @Override
        public RitualRecipe fromJson(ResourceLocation id, JsonObject jsonObject) {
            Fluid fluid = BuiltInRegistries.FLUID.get(new ResourceLocation(GsonHelper.getAsString(jsonObject, "fluid")));
            List<Ingredient> ingredients = new ArrayList<>();
            var ingredientsJson = jsonObject.getAsJsonArray("ingredients");
            for (var ingredientJson : ingredientsJson) {
                ingredients.add(Ingredient.fromJson(ingredientJson));
            }
            Either<ResourceKey<RitualArae>, TagKey<RitualArae>> acceptableAraes;
            String acceptableAraesString = GsonHelper.getAsString(jsonObject, "acceptable_araes");
            if (acceptableAraesString.startsWith("#")) {
                acceptableAraes = Either.right(TagKey.create(RitualArae.REGISTRY_KEY, new ResourceLocation(acceptableAraesString.substring(1))));
            } else {
                acceptableAraes = Either.left(ResourceKey.create(RitualArae.REGISTRY_KEY, new ResourceLocation(acceptableAraesString)));
            }
            DFunction<Boolean> canPlayerUse = DFunction.PREDICATE.codec().decode(JsonOps.INSTANCE, GsonHelper.getAsJsonObject(jsonObject, "can_player_use"))
                    .map(Pair::getFirst)
                    .flatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY))
                    .getOrThrow(false, e -> Haema.LOGGER.error("Error decoding DFunction for Ritual: {}", e));
            //TODO ritual actions
            return new RitualRecipe(id, fluid, ingredients, acceptableAraes, canPlayerUse, List.of());
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
            friendlyByteBuf.writeCharSequence(recipe.acceptableAraes().map(r -> r.location().toString(), t -> "#"+ t.location()), StandardCharsets.UTF_8);
            //TODO finish
        }
    }
}
