package com.williambl.haema.craft.ritual

import com.google.gson.JsonObject
import net.minecraft.fluid.Fluid
import net.minecraft.item.ItemStack
import net.minecraft.network.PacketByteBuf
import net.minecraft.recipe.Ingredient
import net.minecraft.recipe.Recipe
import net.minecraft.recipe.RecipeSerializer
import net.minecraft.recipe.RecipeType
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry
import net.minecraft.world.World

class RitualRecipe(private val ingredients: List<Ingredient>, val fluid: Fluid, private val id: Identifier)
    : Recipe<RitualInventory> {

    fun matches(inv: RitualInventory): Boolean {
        val invItemStacks = inv.getAllItemStacks().toMutableList()

        for (ingredient: Ingredient in ingredients) {
            var result = ItemStack.EMPTY
            for (itemStack: ItemStack in invItemStacks) {
                if (ingredient.test(itemStack)) {
                    result = itemStack
                }
            }

            if (!result.isEmpty) {
                invItemStacks.remove(result)
            } else return false
        }

        return fluid == inv.fluid
    }

    override fun matches(inv: RitualInventory, world: World): Boolean {
        return matches(inv)
    }

    override fun craft(inv: RitualInventory): ItemStack {
        if (matches(inv)) {
            println(inv.player.name.asString())
        }

        return ItemStack.EMPTY
    }

    override fun fits(width: Int, height: Int): Boolean = true

    override fun getOutput(): ItemStack = ItemStack.EMPTY

    override fun getId(): Identifier = id

    override fun getSerializer(): RecipeSerializer<*> = recipeSerializer

    override fun getType(): RecipeType<*> = recipeType

    companion object {
        val recipeType: RecipeType<RitualRecipe> = RecipeType.register("haema:ritual")
        val recipeSerializer: Serializer = RecipeSerializer.register("haema:ritual", Serializer)

        object Serializer: RecipeSerializer<RitualRecipe> {
            override fun read(id: Identifier, json: JsonObject): RitualRecipe {
                return RitualRecipe(json.getAsJsonArray("ingredients").map(Ingredient::fromJson), Registry.FLUID.get(Identifier(json.get("fluid").asString)), id)
            }

            override fun read(id: Identifier, buf: PacketByteBuf): RitualRecipe {
                val ingredientsCount = buf.readInt()
                val ingredients = List(ingredientsCount) { Ingredient.fromPacket(buf) }
                val fluid = Registry.FLUID.get(buf.readIdentifier())
                return RitualRecipe(ingredients, fluid, id)
            }

            override fun write(buf: PacketByteBuf, recipe: RitualRecipe) {
                buf.writeInt(recipe.ingredients.size)
                recipe.ingredients.forEach { it.write(buf) }
                buf.writeIdentifier(Registry.FLUID.getId(recipe.fluid))
            }

        }
    }
}