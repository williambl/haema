package com.williambl.haema.ritual.craft

import com.google.gson.JsonObject
import com.mojang.serialization.JsonOps
import com.williambl.haema.hasUsedRitual
import com.williambl.haema.ritual.RitualModule
import com.williambl.haema.setHasUsedRitual
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import net.minecraft.block.Blocks
import net.minecraft.fluid.Fluid
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NbtCompound
import net.minecraft.nbt.NbtOps
import net.minecraft.network.PacketByteBuf
import net.minecraft.particle.DustParticleEffect
import net.minecraft.particle.ParticleTypes
import net.minecraft.recipe.Ingredient
import net.minecraft.recipe.Recipe
import net.minecraft.recipe.RecipeSerializer
import net.minecraft.recipe.RecipeType
import net.minecraft.registry.DynamicRegistryManager
import net.minecraft.registry.Registries
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundCategory
import net.minecraft.sound.SoundEvents
import net.minecraft.state.property.Properties
import net.minecraft.util.Identifier
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Direction

import net.minecraft.world.World

class RitualRecipe(
    private val id: Identifier,
    val fluid: Fluid,
    val ingredients: List<Ingredient>,
    val minLevel: Int,
    val isRepeatable: Boolean,
    val actionName: Identifier,
    val actionArg: NbtCompound
)
    : Recipe<RitualInventory> {

    fun matches(inv: RitualInventory): Boolean {
        if (!isRepeatable && (inv.player).hasUsedRitual(id) || inv.level < minLevel)
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

    override fun craft(inv: RitualInventory, registryManager: DynamicRegistryManager): ItemStack? {
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

        (inv.player.world as ServerWorld).spawnParticles(DustParticleEffect.DEFAULT, mutable.x.toDouble()+0.5, mutable.y.toDouble()+1.5, mutable.z.toDouble()+0.5, 10, 0.5, 1.0, 0.5, 0.5)

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
                RitualModule.RITUAL_ACTION_REGISTRY.get(actionName)?.runAction(inv, actionArg.get("data") ?: NbtCompound())
                (inv.player).setHasUsedRitual(id, true)
            }
        }

        return ItemStack.EMPTY
    }

    override fun fits(width: Int, height: Int): Boolean = true

    override fun getOutput(registryManager: DynamicRegistryManager): ItemStack? = ItemStack.EMPTY

    override fun getId(): Identifier = id

    override fun getSerializer(): RecipeSerializer<*> = RitualModule.RITUAL_RECIPE_SERIALIZER

    override fun getType(): RecipeType<*> = RitualModule.RITUAL_RECIPE_TYPE

    private fun World.clearFluidState(pos: BlockPos) {
        val blockState = getBlockState(pos)
        if (blockState.properties.contains(Properties.WATERLOGGED)) {
            setBlockState(pos, blockState.with(Properties.WATERLOGGED, false))
        } else {
            setBlockState(pos, Blocks.AIR.defaultState)
        }
    }

    companion object {
        object Serializer: RecipeSerializer<RitualRecipe> {

            override fun read(id: Identifier, json: JsonObject): RitualRecipe {
                val actionElement = json.getAsJsonObject("action")

                return RitualRecipe(
                    id,
                    Registries.FLUID.get(Identifier(json.get("fluid").asString)),
                    json.getAsJsonArray("ingredients").map(Ingredient::fromJson),
                    json.get("minLevel").asInt,
                    json.get("repeatable").asBoolean,
                    Identifier(actionElement.get("name").asString),
                    (actionElement.get("arg") ?: JsonObject()).let {
                        val nbt = JsonOps.INSTANCE.convertTo(NbtOps.INSTANCE, it)
                        val wrapper = NbtCompound()
                        wrapper.put("data", nbt)
                        return@let wrapper
                    }
                )
            }

            override fun read(id: Identifier, buf: PacketByteBuf): RitualRecipe {
                val ingredientsCount = buf.readInt()
                val ingredients = List(ingredientsCount) { Ingredient.fromPacket(buf) }

                return RitualRecipe(
                    id,
                    Registries.FLUID.get(buf.readIdentifier()),
                    ingredients,
                    buf.readInt(),
                    buf.readBoolean(),
                    buf.readIdentifier(),
                    buf.readNbt() ?: NbtCompound()
                )
            }

            override fun write(buf: PacketByteBuf, recipe: RitualRecipe) {
                buf.writeInt(recipe.ingredients.size)
                recipe.ingredients.forEach { it.write(buf) }
                buf.writeIdentifier(Registries.FLUID.getId(recipe.fluid))
                buf.writeInt(recipe.minLevel)
                buf.writeBoolean(recipe.isRepeatable)
                buf.writeIdentifier(recipe.actionName)
                buf.writeNbt(recipe.actionArg)
            }
        }
    }
}