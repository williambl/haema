package com.williambl.haema.craft.ritual

import com.google.gson.JsonObject
import com.williambl.haema.Vampirable
import com.williambl.haema.VampireAbility
import com.williambl.haema.ritual.RitualTableScreenHandler
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import net.minecraft.block.Blocks
import net.minecraft.fluid.Fluid
import net.minecraft.item.ItemStack
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.particle.ParticleTypes
import net.minecraft.recipe.Ingredient
import net.minecraft.recipe.Recipe
import net.minecraft.recipe.RecipeSerializer
import net.minecraft.recipe.RecipeType
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.state.property.Properties
import net.minecraft.util.Identifier
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Direction
import net.minecraft.util.registry.Registry
import net.minecraft.world.World

class RitualRecipe(
    private val id: Identifier,
    val fluid: Fluid,
    val ingredients: List<Ingredient>,
    val minLevel: Int,
    val isRepeatable: Boolean,
    val actionName: String,
    val actionArg: Int
)
    : Recipe<RitualInventory> {

    fun matches(inv: RitualInventory): Boolean {
        if (!isRepeatable && (inv.player as Vampirable).hasUsedRitual(id) || inv.level < minLevel)
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

        (inv.player.world as ServerWorld).spawnParticles(DustParticleEffect.RED, mutable.x.toDouble()+0.5, mutable.y.toDouble()+1.5, mutable.z.toDouble()+0.5, 10, 0.5, 1.0, 0.5, 0.5)

        (inv.player.world as ServerWorld).playSound(null, mutable.x.toDouble()+0.5, mutable.y.toDouble()+1.5, mutable.z.toDouble()+0.5, SoundEvents.ENTITY_GENERIC_EXTINGUISH_FIRE, SoundCategory.BLOCKS, 1f, 1f)
        (inv.player.world as ServerWorld).playSound(null, mutable.x.toDouble()+0.5, mutable.y.toDouble()+1.5, mutable.z.toDouble()+0.5, SoundEvents.BLOCK_END_GATEWAY_SPAWN, SoundCategory.BLOCKS, 1f, 1f)
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.LARGE_SMOKE, mutable.x.toDouble()+0.5, mutable.y.toDouble()+1.5, mutable.z.toDouble()+0.5, 10, 1.5, 1.0, 1.5, 0.2)

        inv.player.world.clearFluidState(mutable.move(Direction.EAST))
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.SOUL, mutable.x.toDouble()+0.5, mutable.y.toDouble()+0.5, mutable.z.toDouble()+0.5, 5, 0.5, 0.5, 0.5, 0.5)
        inv.player.world.clearFluidState(mutable.move(Direction.NORTH))
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.SOUL, mutable.x.toDouble()+0.5, mutable.y.toDouble()+0.5, mutable.z.toDouble()+0.5, 5, 0.5, 0.5, 0.5, 0.5)
        inv.player.world.clearFluidState(mutable.move(Direction.WEST))
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.SOUL, mutable.x.toDouble()+0.5, mutable.y.toDouble()+0.5, mutable.z.toDouble()+0.5, 5, 0.5, 0.5, 0.5, 0.5)
        inv.player.world.clearFluidState(mutable.move(Direction.WEST))
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.SOUL, mutable.x.toDouble()+0.5, mutable.y.toDouble()+0.5, mutable.z.toDouble()+0.5, 5, 0.5, 0.5, 0.5, 0.5)
        inv.player.world.clearFluidState(mutable.move(Direction.SOUTH))
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.SOUL, mutable.x.toDouble()+0.5, mutable.y.toDouble()+0.5, mutable.z.toDouble()+0.5, 5, 0.5, 0.5, 0.5, 0.5)
        inv.player.world.clearFluidState(mutable.move(Direction.SOUTH))
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.SOUL, mutable.x.toDouble()+0.5, mutable.y.toDouble()+0.5, mutable.z.toDouble()+0.5, 5, 0.5, 0.5, 0.5, 0.5)
        inv.player.world.clearFluidState(mutable.move(Direction.EAST))
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.SOUL, mutable.x.toDouble()+0.5, mutable.y.toDouble()+0.5, mutable.z.toDouble()+0.5, 5, 0.5, 0.5, 0.5, 0.5)
        inv.player.world.clearFluidState(mutable.move(Direction.EAST))
        (inv.player.world as ServerWorld).spawnParticles(ParticleTypes.SOUL, mutable.x.toDouble()+0.5, mutable.y.toDouble()+0.5, mutable.z.toDouble()+0.5, 5, 0.5, 0.5, 0.5, 0.5)

        GlobalScope.launch {
            delay(200)
            (inv.player.world as ServerWorld).server.submit {
                ritualActions[actionName]?.invoke(inv, actionArg)
                (inv.player as Vampirable).setHasUsedRitual(id, true)
            }
        }

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
                (inv.player as Vampirable).setAbilityLevel(VampireAbility.NONE, (inv.player as Vampirable).getAbilityLevel(VampireAbility.NONE)+arg)
                inv.player.openHandledScreen(RitualTableScreenHandler.Factory(inv))
            },
            "change_abilities" to { inv, _ -> inv.player.openHandledScreen(RitualTableScreenHandler.Factory(inv)) }
        )

        object Serializer: RecipeSerializer<RitualRecipe> {

            override fun read(id: Identifier, json: JsonObject): RitualRecipe {
                val actionElement = json.getAsJsonObject("action")

                return RitualRecipe(
                    id,
                    Registry.FLUID.get(Identifier(json.get("fluid").asString)),
                    json.getAsJsonArray("ingredients").map(Ingredient::fromJson),
                    json.get("minLevel").asInt,
                    json.get("repeatable").asBoolean,
                    actionElement.get("name").asString,
                    actionElement.get("arg")?.asInt ?: 0
                )
            }

            override fun read(id: Identifier, buf: PacketByteBuf): RitualRecipe {
                val ingredientsCount = buf.readInt()
                val ingredients = List(ingredientsCount) { Ingredient.fromPacket(buf) }

                return RitualRecipe(
                    id,
                    Registry.FLUID.get(buf.readIdentifier()),
                    ingredients,
                    buf.readInt(),
                    buf.readBoolean(),
                    buf.readString(),
                    buf.readInt()
                )
            }

            override fun write(buf: PacketByteBuf, recipe: RitualRecipe) {
                buf.writeInt(recipe.ingredients.size)
                recipe.ingredients.forEach { it.write(buf) }
                buf.writeIdentifier(Registry.FLUID.getId(recipe.fluid))
                buf.writeInt(recipe.minLevel)
                buf.writeBoolean(recipe.isRepeatable)
                buf.writeString(recipe.actionName)
                buf.writeInt(recipe.actionArg)
            }
        }
    }
}