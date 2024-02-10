package com.williambl.haema.content.injector;

import com.google.gson.JsonObject;
import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import com.williambl.haema.api.content.blood.BloodQuality;
import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodFluid;
import com.williambl.haema.vampire.HaemaVampires;
import net.fabricmc.fabric.api.transfer.v1.context.ContainerItemContext;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidStorage;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.item.InventoryStorage;
import net.fabricmc.fabric.api.transfer.v1.storage.Storage;
import net.fabricmc.fabric.api.transfer.v1.storage.StorageUtil;
import net.fabricmc.fabric.api.transfer.v1.storage.base.CombinedStorage;
import net.fabricmc.fabric.api.transfer.v1.transaction.Transaction;
import net.minecraft.advancements.Advancement;
import net.minecraft.advancements.AdvancementRequirements;
import net.minecraft.advancements.AdvancementRewards;
import net.minecraft.advancements.Criterion;
import net.minecraft.advancements.critereon.RecipeUnlockedTrigger;
import net.minecraft.core.NonNullList;
import net.minecraft.core.RegistryAccess;
import net.minecraft.data.recipes.RecipeBuilder;
import net.minecraft.data.recipes.RecipeOutput;
import net.minecraft.data.recipes.ShapelessRecipeBuilder;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.GsonHelper;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.CustomRecipe;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public class BloodFillingRecipe extends CustomRecipe {
    private final long amountNeeded;
    private final Ingredient empty;

    public BloodFillingRecipe(CraftingBookCategory craftingBookCategory, long amountNeeded, Ingredient empty) {
        super(craftingBookCategory);
        this.amountNeeded = amountNeeded;
        this.empty = empty;
    }

    @Override
    public boolean matches(CraftingContainer container, Level level) {
        return craft(container).matches();
    }

    @Override
    public ItemStack assemble(CraftingContainer container, RegistryAccess registryAccess) {
        return craft(container).result();
    }

    @Override
    public NonNullList<ItemStack> getRemainingItems(CraftingContainer container) {
        return craft(container).remainder();
    }

    private CraftResult craft(CraftingContainer container) {
        ItemStack injector = ItemStack.EMPTY;
        int injectorSlot = -1;
        List<ItemStack> sources = new ArrayList<>();
        List<Storage<FluidVariant>> storages = new ArrayList<>();
        var containerCopy = new SimpleContainer(container.getContainerSize());
        var containerCopyStorage = InventoryStorage.of(containerCopy, null);

        // make sure all items are valid
        for(int i = 0; i < container.getContainerSize(); ++i) {
            ItemStack stack = container.getItem(i).copy();
            containerCopy.setItem(i, stack);
            if (!stack.isEmpty()) {
                if (this.empty.test(stack)) {
                    if (!injector.isEmpty()) {
                        return CraftResult.noMatch(container);
                    }

                    injector = stack;
                    injectorSlot = i;
                } else {
                    var storage = ContainerItemContext.ofSingleSlot(containerCopyStorage.getSlot(i)).find(FluidStorage.ITEM);
                    if (storage == null) {
                        return CraftResult.noMatch(container);
                    }

                    sources.add(stack);
                    storages.add(storage);
                }
            }
        }

        if (injector.isEmpty() || sources.isEmpty() || storages.isEmpty()) {
            return CraftResult.noMatch(container);
        }

        // make sure all sources give same fluid
        Fluid fluid = null;
        storageLoop: for (var storage : storages) {
            for (var view : storage.nonEmptyViews()) {
                if (view.getResource().getFluid() instanceof BloodFluid containedFluid) {
                    if (fluid == null) {
                        fluid = containedFluid;
                        continue storageLoop;
                    } else if (fluid == containedFluid) {
                        continue storageLoop;
                    }
                }
            }
            return CraftResult.noMatch(container);
        }

        var combinedStorage = new CombinedStorage<>(storages);
        var injectorStorage = ContainerItemContext.ofSingleSlot(containerCopyStorage.getSlot(injectorSlot)).find(FluidStorage.ITEM);
        try (var transaction = Transaction.openOuter()) {
            if (StorageUtil.move(combinedStorage, injectorStorage, FluidVariant.of(fluid)::equals, this.amountNeeded, transaction) == this.amountNeeded) {
                transaction.commit();
                var result = containerCopy.removeItem(injectorSlot, 1);
                var list = NonNullList.withSize(container.getContainerSize(), ItemStack.EMPTY);
                for (int i = 0; i < container.getContainerSize(); i++) {
                    list.set(i, containerCopy.getItem(i));
                }
                return new CraftResult(true, result, list);
            }
            transaction.abort();
        }

        return CraftResult.noMatch(container);
    }

    @Override
    public boolean canCraftInDimensions(int i, int j) {
        return i * j >= 2;
    }

    @Override
    public RecipeSerializer<?> getSerializer() {
        return null;
    }

    private record CraftResult(boolean matches, ItemStack result, NonNullList<ItemStack> remainder) {
        private static CraftResult noMatch(CraftingContainer container) {
            return new CraftResult(false, ItemStack.EMPTY, NonNullList.withSize(container.getContainerSize(), ItemStack.EMPTY));
        }
    }

    // why can't they use a codec for the JSON stuff :(
    public static class Builder implements RecipeBuilder {
        private long amountNeeded = HaemaContent.ContentConstants.INJECTOR_CAPACITY_DROPLETS;
        private Ingredient empty = Ingredient.EMPTY;

        private final Map<String, Criterion<?>> criteria = new LinkedHashMap();
        @Nullable
        private String group;
        private CraftingBookCategory category;

        private Builder() {
        }

        public static Builder create() {
            return new Builder();
        }

        public Builder amountNeeded(long amountNeeded) {
            this.amountNeeded = amountNeeded;
            return this;
        }

        public Builder empty(Ingredient empty) {
            this.empty = empty;
            return this;
        }

        @Override
        public void save(RecipeOutput consumer, ResourceLocation id) {
            Advancement.Builder builder = consumer.advancement().addCriterion("has_the_recipe", RecipeUnlockedTrigger.unlocked(id)).rewards(AdvancementRewards.Builder.recipe(id)).requirements(AdvancementRequirements.Strategy.OR);
            this.criteria.forEach(builder::addCriterion);
            consumer.accept(id, new BloodFillingRecipe(this.category, this.amountNeeded, this.empty), builder.build(id.withPrefix("recipes/" + this.category.getSerializedName() + "/")));
        }

        @Override
        public BloodFillingRecipe.Builder unlockedBy(String string, Criterion<?> criterion) {
            this.criteria.put(string, criterion);
            return this;
        }

        @Override
        public BloodFillingRecipe.Builder group(@Nullable String string) {
            this.group = string;
            return this;
        }


        public BloodFillingRecipe.Builder category(CraftingBookCategory category) {
            this.category = category;
            return this;
        }

        @Override
        public Item getResult() {
            return HaemaContent.ContentItems.INJECTORS.get(BloodQuality.GOOD);
        }
    }

    public static class Serializer implements RecipeSerializer<BloodFillingRecipe> {
        private static final Codec<BloodFillingRecipe> CODEC = RecordCodecBuilder.create(instance -> instance.group(
                CraftingBookCategory.CODEC.fieldOf("category").forGetter(CustomRecipe::category),
                Codec.LONG.fieldOf("amount_needed").forGetter(r -> r.amountNeeded),
                Ingredient.CODEC_NONEMPTY.fieldOf("empty").forGetter(r -> r.empty)
        ).apply(instance, BloodFillingRecipe::new));

        @Override
        public Codec<BloodFillingRecipe> codec() {
            return CODEC;
        }

        @Override
        public BloodFillingRecipe fromNetwork(FriendlyByteBuf friendlyByteBuf) {
            CraftingBookCategory craftingBookCategory = friendlyByteBuf.readEnum(CraftingBookCategory.class);
            long amountNeeded = friendlyByteBuf.readVarLong();
            Ingredient empty = Ingredient.fromNetwork(friendlyByteBuf);
            return new BloodFillingRecipe(craftingBookCategory, amountNeeded, empty);
        }

        @Override
        public void toNetwork(FriendlyByteBuf friendlyByteBuf, BloodFillingRecipe recipe) {
            friendlyByteBuf.writeEnum(recipe.category());
            friendlyByteBuf.writeVarLong(recipe.amountNeeded);
            recipe.empty.toNetwork(friendlyByteBuf);
        }
    }
}
