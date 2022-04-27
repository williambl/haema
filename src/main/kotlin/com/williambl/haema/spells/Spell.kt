package com.williambl.haema.spells

import net.minecraft.entity.LivingEntity
import net.minecraft.text.MutableText
import net.minecraft.text.TranslatableText
import net.minecraft.util.Util
import net.minecraft.world.World

abstract class Spell {
    private val translationKey: String by lazy { Util.createTranslationKey("spell", SpellsModule.SPELL_REGISTRY.getId(this)) }

    fun getName(): MutableText = TranslatableText(translationKey)

    abstract val chargeTime: Int
    abstract fun createChargeParticles(world: World, user: LivingEntity, ticksRemaining: Int)

    abstract fun use(world: World, user: LivingEntity)
}
