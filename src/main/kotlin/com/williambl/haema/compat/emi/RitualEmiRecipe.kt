package com.williambl.haema.compat.emi

import com.williambl.haema.ritual.RitualModule
import com.williambl.haema.ritual.craft.RitualRecipe
import dev.emi.emi.api.recipe.EmiRecipe
import dev.emi.emi.api.recipe.EmiRecipeCategory
import dev.emi.emi.api.stack.EmiIngredient
import dev.emi.emi.api.stack.EmiStack
import dev.emi.emi.api.widget.WidgetHolder
import net.fabricmc.fabric.api.transfer.v1.fluid.FluidConstants
import net.minecraft.client.MinecraftClient
import net.minecraft.nbt.NbtCompound
import net.minecraft.text.OrderedText
import net.minecraft.text.Text
import net.minecraft.util.Formatting
import net.minecraft.util.Identifier

class RitualEmiRecipe(private val recipe: RitualRecipe): EmiRecipe {
    @Suppress("UnstableApiUsage")
    private val inputIngredients = this.recipe.ingredients.map(EmiIngredient::of).toMutableList().also { it.add(EmiStack.of(this.recipe.fluid, FluidConstants.BUCKET * 8)) }

    override fun getCategory(): EmiRecipeCategory = RitualCategory

    override fun getId(): Identifier = this.recipe.id

    override fun getInputs(): MutableList<EmiIngredient> = this.inputIngredients.toMutableList()

    override fun getOutputs(): MutableList<EmiStack> = mutableListOf()

    override fun getDisplayWidth(): Int = 150

    override fun getDisplayHeight(): Int = 60

    override fun addWidgets(widgets: WidgetHolder) {
        val inputsPoint = Pair(widgets.width/2 - 64, widgets.height/2 - 16)
        val outputPoint = Pair(widgets.width/2 + 20, widgets.height/2 - 8)

        val inputEntries = this.inputIngredients
        inputEntries.forEachIndexed { i, ingredient ->
            widgets.addSlot(ingredient, inputsPoint.first+((i+1)%2)*18, inputsPoint.second+(i/2)*18)
        }

        this.addCenteredText(
            widgets,
            (Text.translatable("gui.haema.requires").append(Text.translatable("gui.haema.altar_level."+this.recipe.minLevel))).formatted(
                Formatting.UNDERLINE).formatted(Formatting.DARK_GRAY).asOrderedText(),
            widgets.width/2, widgets.height/2 - 28,
            0xffffff,
            false
        )

        this.addCenteredText(
            widgets,
            Text.translatable("gui.haema.repeatable.${this.recipe.isRepeatable}").formatted(if (this.recipe.isRepeatable) Formatting.DARK_GREEN else Formatting.DARK_RED).asOrderedText(),
            widgets.width/2+16, widgets.height/2+16,
            0xffffff,
            false
        )

        this.addCenteredText(
            widgets,
            (RitualModule.RITUAL_ACTION_REGISTRY.get(this.recipe.actionName)?.getName(this.recipe.actionArg.get("data") ?: NbtCompound()) ?: Text.literal("?")).formatted(Formatting.DARK_GRAY).asOrderedText(),
            outputPoint.first, outputPoint.second,
            0xffffff,
            false
        )
    }

    private fun addCenteredText(widgets: WidgetHolder, text: OrderedText, x: Int, y: Int, colour: Int, shadow: Boolean) {
        widgets.addText(
            text,
            x - MinecraftClient.getInstance().textRenderer.getWidth(text)/2,
            y,
            colour,
            shadow
        )
    }
}