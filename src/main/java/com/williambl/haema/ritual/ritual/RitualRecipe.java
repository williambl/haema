package com.williambl.haema.ritual.ritual;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.mojang.datafixers.util.Either;
import com.mojang.datafixers.util.Pair;
import com.mojang.serialization.Codec;
import com.mojang.serialization.JsonOps;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.dfunc.api.DFunction;
import com.williambl.dfunc.api.context.DFContext;
import com.williambl.dfunc.api.context.DFContextSpec;
import com.williambl.dfunc.api.functions.DPredicates;
import com.williambl.haema.Haema;
import com.williambl.haema.HaemaUtil;
import com.williambl.haema.api.ritual.RitualArae;
import com.williambl.haema.ritual.HaemaRituals;
import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.CriterionTriggerInstance;
import net.minecraft.advancements.RequirementsStrategy;
import net.minecraft.advancements.critereon.RecipeUnlockedTrigger;
import net.minecraft.core.*;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.data.recipes.FinishedRecipe;
import net.minecraft.data.recipes.RecipeBuilder;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.RegistryOps;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.util.GsonHelper;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import xyz.nucleoid.codecs.MoreCodecs;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;

public sealed abstract class RitualRecipe implements Recipe<RitualContainer> {
    private final ResourceLocation id;
    private final String group;

    protected RitualRecipe(ResourceLocation id, String group) {
        this.id = id;
        this.group = group;
    }

    protected abstract Data data();

    protected abstract boolean isAraeAcceptable(RitualArae arae, Registry<RitualArae> registryOrThrow);

    @Override
    public @NotNull String getGroup() {
        return this.group;
    }

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

        private Server(Data.ServerData data, ResourceLocation id, String group) {
            super(id, group);
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

        private Client(Data.ClientData data, ResourceLocation id, String group) {
            super(id, group);
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
            String group = GsonHelper.getAsString(jsonObject, "group", "");
            return new RitualRecipe.Server(partial, id, group);
        }

        @Override
        public RitualRecipe fromNetwork(ResourceLocation id, FriendlyByteBuf friendlyByteBuf) {
            String group = friendlyByteBuf.readUtf();
            return new RitualRecipe.Client(Data.ClientData.fromNetwork(friendlyByteBuf), id, group);
        }

        @Override
        public void toNetwork(FriendlyByteBuf friendlyByteBuf, RitualRecipe recipe) {
            friendlyByteBuf.writeUtf(recipe.getGroup());
            recipe.data().toNetwork(friendlyByteBuf);
        }
    }

    public static class Builder implements RecipeBuilder {
        private final RegistryAccess registries;
        private final List<Ingredient> ingredients = new ArrayList<>();
        private final List<RitualAction> actions = new ArrayList<>();
        private final Advancement.Builder advancement = Advancement.Builder.advancement();
        private DFunction<Boolean> canPlayerUse = DPredicates.CONSTANT.factory().apply(true);
        private Fluid fluid = Fluids.EMPTY;
        private Either<TagKey<RitualArae>, List<ResourceKey<RitualArae>>> acceptableAraes = null;
        private String group;

        public static Builder ritual(RegistryAccess registries) {
            return new Builder(registries);
        }

        private Builder(RegistryAccess registries) {
            this.registries = registries;
        }

        @Override
        public Builder unlockedBy(String string, CriterionTriggerInstance criterionTriggerInstance) {
            this.advancement.addCriterion(string, criterionTriggerInstance);
            return this;
        }

        @Override
        public Builder group(@Nullable String string) {
            this.group = string;
            return this;
        }

        public Builder fluid(Fluid fluid) {
            this.fluid = fluid;
            return this;
        }

        @SafeVarargs
        public final Builder acceptableAraes(ResourceKey<RitualArae>... araes) {
            this.acceptableAraes = Either.right(Arrays.asList(araes));
            return this;
        }

        public Builder acceptableAraes(Collection<ResourceKey<RitualArae>> araes) {
            this.acceptableAraes = Either.right(List.copyOf(araes));
            return this;
        }

        public Builder acceptableAraes(TagKey<RitualArae> tag) {
            this.acceptableAraes = Either.left(tag);
            return this;
        }

        public Builder canPlayerUse(DFunction<Boolean> dfunction) {
            this.canPlayerUse = dfunction;
            return this;
        }

        @Override
        public Item getResult() {
            return Items.AIR;
        }

        private void ensureValid(ResourceLocation resourceLocation) {
            if (this.acceptableAraes == null) {
                throw new IllegalStateException("No acceptable araes are set for ritual "+resourceLocation);
            }
        }

        @Override
        public void save(Consumer<FinishedRecipe> consumer, ResourceLocation resourceLocation) {
            this.ensureValid(resourceLocation);
            this.advancement
                    .parent(ROOT_RECIPE_ADVANCEMENT)
                    .addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(resourceLocation))
                    .rewards(AdvancementRewards.Builder.recipe(resourceLocation))
                    .requirements(RequirementsStrategy.OR);
            var araeRegistry = this.registries.registryOrThrow(RitualArae.REGISTRY_KEY);
            consumer.accept(
                    new Result(
                            resourceLocation,
                            this.group == null ? "" : this.group,
                            this.advancement,
                            resourceLocation.withPrefix("recipes/rituals/"),
                            new Data.ServerData(
                                    this.fluid,
                                    this.ingredients,
                                    this.acceptableAraes.map(araeRegistry::getOrCreateTag, l -> HolderSet.direct(l.stream().map(araeRegistry::getHolderOrThrow).toList())),
                                    this.canPlayerUse,
                                    this.actions
                            ),
                            this.registries));

        }

        private static class Result implements FinishedRecipe {
            private final ResourceLocation id;
            private final String group;
            private final Advancement.Builder advancement;
            private final ResourceLocation advancementId;
            private final Data.ServerData data;
            private final RegistryOps<JsonElement> ops;

            public Result(ResourceLocation id, String group, Advancement.Builder advancement, ResourceLocation advancementId, Data.ServerData data, RegistryAccess registries) {
                this.id = id;
                this.group = group;
                this.advancement = advancement;
                this.advancementId = advancementId;
                this.data = data;
                this.ops = RegistryOps.create(JsonOps.INSTANCE, registries);
            }

            @Override
            public void serializeRecipeData(JsonObject jsonObject) {
                jsonObject.addProperty("group", this.group);
                Data.ServerData.CODEC.encode(this.data, this.ops, jsonObject);
            }

            @Override
            public ResourceLocation getId() {
                return this.id;
            }

            @Override
            public RecipeSerializer<?> getType() {
                return HaemaRituals.RitualRecipeSerializers.RITUAL;
            }

            @Nullable
            @Override
            public JsonObject serializeAdvancement() {
                return this.advancement.serializeToJson();
            }

            @Nullable
            @Override
            public ResourceLocation getAdvancementId() {
                return this.advancementId;
            }
        }
    }
}
