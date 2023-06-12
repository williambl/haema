package com.williambl.haema.content.injector;

import com.google.gson.JsonObject;
import com.williambl.haema.content.HaemaContent;
import com.williambl.haema.content.blood.BloodFluid;
import net.fabricmc.fabric.api.transfer.v1.context.ContainerItemContext;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidStorage;
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidVariant;
import net.fabricmc.fabric.api.transfer.v1.item.InventoryStorage;
import net.fabricmc.fabric.api.transfer.v1.storage.Storage;
import net.fabricmc.fabric.api.transfer.v1.storage.StorageUtil;
import net.fabricmc.fabric.api.transfer.v1.storage.base.CombinedStorage;
import net.fabricmc.fabric.api.transfer.v1.transaction.Transaction;
import net.minecraft.core.NonNullList;
import net.minecraft.core.RegistryAccess;
import net.minecraft.data.recipes.CraftingRecipeBuilder;
import net.minecraft.data.recipes.FinishedRecipe;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.GsonHelper;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingBookCategory;
import net.minecraft.world.item.crafting.CustomRecipe;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

@SuppressWarnings("UnstableApiUsage")
public class BloodFillingRecipe extends CustomRecipe {
    private final long amountNeeded;
    private final Ingredient empty;

    public BloodFillingRecipe(ResourceLocation resourceLocation, CraftingBookCategory craftingBookCategory, long amountNeeded, Ingredient empty) {
        super(resourceLocation, craftingBookCategory);
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
    public static class Builder extends CraftingRecipeBuilder {
        final RecipeSerializer<?> serializer;
        long amountNeeded = HaemaContent.Config.INJECTOR_CAPACITY_DROPLETS;
        Ingredient empty = Ingredient.EMPTY;

        public Builder(RecipeSerializer<?> recipeSerializer) {
            this.serializer = recipeSerializer;
        }

        public static Builder create() {
            return new Builder(HaemaContent.Recipes.BLOOD_FILLING_SERIALIZER);
        }

        public Builder amountNeeded(long amountNeeded) {
            this.amountNeeded = amountNeeded;
            return this;
        }

        public Builder empty(Ingredient empty) {
            this.empty = empty;
            return this;
        }

        public void save(Consumer<FinishedRecipe> consumer, ResourceLocation id) {
            consumer.accept(new CraftingRecipeBuilder.CraftingResult(CraftingBookCategory.MISC) {
                @Override
                public void serializeRecipeData(JsonObject jsonObject) {
                    super.serializeRecipeData(jsonObject);
                    jsonObject.addProperty("amount_needed", Builder.this.amountNeeded);
                    jsonObject.add("empty", Builder.this.empty.toJson());
                }

                @Override
                public RecipeSerializer<?> getType() {
                    return Builder.this.serializer;
                }

                @Override
                public ResourceLocation getId() {
                    return id;
                }

                @Nullable
                @Override
                public JsonObject serializeAdvancement() {
                    return null;
                }

                @Override
                public ResourceLocation getAdvancementId() {
                    return new ResourceLocation("");
                }
            });
        }
    }

    public static class Serializer implements RecipeSerializer<BloodFillingRecipe> {
        @Override
        public BloodFillingRecipe fromJson(ResourceLocation resourceLocation, JsonObject jsonObject) {
            CraftingBookCategory craftingBookCategory = CraftingBookCategory.CODEC
                    .byName(GsonHelper.getAsString(jsonObject, "category", null), CraftingBookCategory.MISC);
            long amountNeeded = GsonHelper.getAsLong(jsonObject, "amount_needed");
            Ingredient empty = Ingredient.fromJson(GsonHelper.getAsJsonObject(jsonObject, "empty"));
            return new BloodFillingRecipe(resourceLocation, craftingBookCategory, amountNeeded, empty);
        }

        @Override
        public BloodFillingRecipe fromNetwork(ResourceLocation resourceLocation, FriendlyByteBuf friendlyByteBuf) {
            CraftingBookCategory craftingBookCategory = friendlyByteBuf.readEnum(CraftingBookCategory.class);
            long amountNeeded = friendlyByteBuf.readVarLong();
            Ingredient empty = Ingredient.fromNetwork(friendlyByteBuf);
            return new BloodFillingRecipe(resourceLocation, craftingBookCategory, amountNeeded, empty);
        }

        @Override
        public void toNetwork(FriendlyByteBuf friendlyByteBuf, BloodFillingRecipe recipe) {
            friendlyByteBuf.writeEnum(recipe.category());
            friendlyByteBuf.writeVarLong(recipe.amountNeeded);
            recipe.empty.toNetwork(friendlyByteBuf);
        }
    }
}
