package com.williambl.haema.craft.ritual

import com.google.gson.JsonObject
import com.williambl.haema.Vampirable
import com.williambl.haema.VampireAbility
import net.minecraft.block.Blocks
import net.minecraft.fluid.Fluid
import net.minecraft.item.ItemStack
import net.minecraft.network.PacketByteBuf
import net.minecraft.recipe.Ingredient
import net.minecraft.recipe.Recipe
import net.minecraft.recipe.RecipeSerializer
import net.minecraft.recipe.RecipeType
import net.minecraft.state.property.Properties
import net.minecraft.util.Identifier
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Direction
import net.minecraft.util.registry.Registry
import net.minecraft.world.World

class RitualRecipe(
    private val id: Identifier,
    val fluid: Fluid,
    private val ingredients: List<Ingredient>,
    val isRepeatable: Boolean,
    val actionName: String,
    val actionArg: Int
)
    : Recipe<RitualInventory> {

    fun matches(inv: RitualInventory): Boolean {
        if (!isRepeatable && (inv.player as Vampirable).hasUsedRitual(id))
            return false

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
        if (!matches(inv)) {
            return ItemStack.EMPTY
        }

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
                inv.removeItem(result.item, 1)
            }
        }

        val mutable = inv.pos.mutableCopy().move(Direction.DOWN)

        inv.player.world.clearFluidState(mutable.move(Direction.EAST))
        inv.player.world.clearFluidState(mutable.move(Direction.NORTH))
        inv.player.world.clearFluidState(mutable.move(Direction.WEST))
        inv.player.world.clearFluidState(mutable.move(Direction.WEST))
        inv.player.world.clearFluidState(mutable.move(Direction.SOUTH))
        inv.player.world.clearFluidState(mutable.move(Direction.SOUTH))
        inv.player.world.clearFluidState(mutable.move(Direction.EAST))
        inv.player.world.clearFluidState(mutable.move(Direction.EAST))

        ritualActions[actionName]?.invoke(inv, actionArg)
        (inv.player as Vampirable).setHasUsedRitual(id, true)

        return ItemStack.EMPTY
    }

    override fun fits(width: Int, height: Int): Boolean = true

    override fun getOutput(): ItemStack = ItemStack.EMPTY

    override fun getId(): Identifier = id

    override fun getSerializer(): RecipeSerializer<*> = recipeSerializer

    override fun getType(): RecipeType<*> = recipeType

    private fun World.clearFluidState(pos: BlockPos) {
        val blockState = getBlockState(pos)
        if (blockState.properties.contains(Properties.WATERLOGGED)) {
            setBlockState(pos, blockState.with(Properties.WATERLOGGED, false))
        } else {
            setBlockState(pos, Blocks.AIR.defaultState)
        }
    }

    companion object {
        val recipeType: RecipeType<RitualRecipe> = RecipeType.register("haema:ritual")
        val recipeSerializer: Serializer = RecipeSerializer.register("haema:ritual", Serializer)

        val ritualActions = mapOf<String, (RitualInventory, Int) -> Unit>(
            "add_level" to { inv, arg ->
                (inv.player as Vampirable).setAbilityLevel(VampireAbility.DASH, arg)
            },
            "change_abilities" to { _, _ -> }
        )

        object Serializer: RecipeSerializer<RitualRecipe> {

            override fun read(id: Identifier, json: JsonObject): RitualRecipe {
                val actionElement = json.getAsJsonObject("action")

                return RitualRecipe(
                    id,
                    Registry.FLUID.get(Identifier(json.get("fluid").asString)),
                    json.getAsJsonArray("ingredients").map(Ingredient::fromJson),
                    json.get("repeatable").asBoolean,
                    actionElement.get("name").asString,
                    actionElement.get("arg").asNumber.toInt()
                )
            }

            override fun read(id: Identifier, buf: PacketByteBuf): RitualRecipe {
                val ingredientsCount = buf.readInt()
                val ingredients = List(ingredientsCount) { Ingredient.fromPacket(buf) }

                return RitualRecipe(
                    id,
                    Registry.FLUID.get(buf.readIdentifier()),
                    ingredients,
                    buf.readBoolean(),
                    buf.readString(),
                    buf.readInt()
                )
            }

            override fun write(buf: PacketByteBuf, recipe: RitualRecipe) {
                buf.writeInt(recipe.ingredients.size)
                recipe.ingredients.forEach { it.write(buf) }
                buf.writeIdentifier(Registry.FLUID.getId(recipe.fluid))
                buf.writeBoolean(recipe.isRepeatable)
                buf.writeString(recipe.actionName)
                buf.writeInt(recipe.actionArg)
            }
        }
    }
}