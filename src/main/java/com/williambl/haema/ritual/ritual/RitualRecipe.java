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
import net.minecraft.core.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;
import xyz.nucleoid.codecs.MoreCodecs;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

public sealed abstract class RitualRecipe implements Recipe<RitualContainer> {
    private final ResourceLocation id;

    protected RitualRecipe(ResourceLocation id) {
        this.id = id;
    }

    protected abstract Data data();

    protected abstract boolean isAraeAcceptable(RitualArae arae, Registry<RitualArae> registryOrThrow);

    @Override
    public boolean matches(RitualContainer container, Level level) {
        return this.isAraeAcceptable(container.arae(), level.registryAccess().registryOrThrow(RitualArae.REGISTRY_KEY))
                && HaemaUtil.allMatchOne(container.items, this.data().ingredients())
                && this.data().fluid().isSame(container.fluid());
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
    public ResourceLocation getId() {
        return this.id;
    }

    @Override
    public ItemStack getResultItem(RegistryAccess registryAccess) {
        return ItemStack.EMPTY;
    }

    @Override
    public RecipeSerializer<?> getSerializer() {
        return HaemaRituals.RitualRecipeSerializers.RITUAL;
    }

    @Override
    public RecipeType<?> getType() {
        return HaemaRituals.RitualRecipeTypes.RITUAL;
    }

    private static final class Server extends RitualRecipe {
        private final Data.ServerData data;

        private Server(Data.ServerData data, ResourceLocation id) {
            super(id);
            this.data = data;
        }

        @Override
        protected Data data() {
            return this.data;
        }

        @Override
        public boolean matches(RitualContainer container, Level level) {
            return this.data.canPlayerUse().apply(DFContext.entity(container.player())) && super.matches(container, level);
        }

        @Override
        protected boolean isAraeAcceptable(RitualArae toCheck, Registry<RitualArae> registry) {
            return this.data.acceptableAraes.contains(registry.wrapAsHolder(toCheck));
        }
    }

    private static final class Client extends RitualRecipe {
        private final Data.ClientData data;

        private Client(Data.ClientData data, ResourceLocation id) {
            super(id);
            this.data = data;
        }

        @Override
        protected Data data() {
            return this.data;
        }

        @Override
        protected boolean isAraeAcceptable(RitualArae arae, Registry<RitualArae> registryOrThrow) {
            return registryOrThrow.getResourceKey(arae).filter(this.data.acceptableAraes()::contains).isPresent();
        }
    }

    private sealed interface Data {
        Fluid fluid();
        List<Ingredient> ingredients();
        List<ResourceKey<RitualArae>> araeKeys();

        default void toNetwork(FriendlyByteBuf buf) {
            buf.writeVarInt(BuiltInRegistries.FLUID.getId(this.fluid()));
            buf.writeVarInt(this.ingredients().size());
            for (var ingredient : this.ingredients()) {
                ingredient.toNetwork(buf);
            }
            var araes = this.araeKeys();
            buf.writeVarInt(araes.size());
            for (var arae : araes) {
                buf.writeResourceKey(arae);
            }
        }

        record ServerData(
                Fluid fluid,
                List<Ingredient> ingredients,
                HolderSet<RitualArae> acceptableAraes,
                DFunction<Boolean> canPlayerUse,
                List<RitualAction> actions
        ) implements Data {

            private static final Codec<ServerData> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                    BuiltInRegistries.FLUID.byNameCodec().fieldOf("fluid").forGetter(ServerData::fluid),
                    MoreCodecs.INGREDIENT.listOf().fieldOf("ingredients").forGetter(ServerData::ingredients),
                    RegistryCodecs.homogeneousList(RitualArae.REGISTRY_KEY).fieldOf("acceptable_araes").forGetter(ServerData::acceptableAraes),
                    DFunction.PREDICATE.codec().comapFlatMap(HaemaUtil.verifyDFunction(DFContextSpec.ENTITY), Function.identity()).fieldOf("can_player_use").forGetter(ServerData::canPlayerUse),
                    RitualAction.ACTION_CODEC.listOf().fieldOf("actions").forGetter(ServerData::actions)
            ).apply(instance, ServerData::new));

            @Override
            public List<ResourceKey<RitualArae>> araeKeys() {
                return this.acceptableAraes().stream().filter(Holder::isBound).flatMap(h -> h.unwrapKey().stream()).toList();
            }
        }

        record ClientData(
                Fluid fluid,
                List<Ingredient> ingredients,
                Set<ResourceKey<RitualArae>> acceptableAraes
        ) implements Data {
            private static ClientData fromNetwork(FriendlyByteBuf buf) {
                Fluid fluid = BuiltInRegistries.FLUID.getHolder(buf.readVarInt()).map(Holder.Reference::unwrap).flatMap(e -> e.right()).orElse(Fluids.EMPTY);
                int ingredientCount = buf.readVarInt();
                List<Ingredient> ingredients = new ArrayList<>();
                for (int i = 0; i < ingredientCount; i++) {
                    ingredients.add(Ingredient.fromNetwork(buf));
                }
                int araeCount = buf.readVarInt();
                List<ResourceKey<RitualArae>> araes = new ArrayList<>();
                for (int i = 0; i < araeCount; i++) {
                    araes.add(buf.readResourceKey(RitualArae.REGISTRY_KEY));
                }
                return new ClientData(fluid, ingredients, Set.copyOf(araes));
            }

            @Override
            public List<ResourceKey<RitualArae>> araeKeys() {
                return List.copyOf(this.acceptableAraes);
            }
        }
    }

    public static class Serializer implements RecipeSerializer<RitualRecipe> {
        @Override
        public RitualRecipe fromJson(ResourceLocation id, JsonObject jsonObject) {
            var partial = Data.ServerData.CODEC.decode(JsonOps.INSTANCE, jsonObject)
                    .map(Pair::getFirst)
                    .getOrThrow(false, e -> Haema.LOGGER.error("Error deserialising ritual {}: {}", id, e));
            return new RitualRecipe.Server(partial, id);
        }

        @Override
        public RitualRecipe fromNetwork(ResourceLocation id, FriendlyByteBuf friendlyByteBuf) {
            return new RitualRecipe.Client(Data.ClientData.fromNetwork(friendlyByteBuf), id);
        }

        @Override
        public void toNetwork(FriendlyByteBuf friendlyByteBuf, RitualRecipe recipe) {
            recipe.data().toNetwork(friendlyByteBuf);
        }
    }
}
