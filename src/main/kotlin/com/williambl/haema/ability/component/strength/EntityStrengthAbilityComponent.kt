package com.williambl.haema.ability.component.strength

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.component.invisibility.InvisibilityAbilityComponent
import com.williambl.haema.criteria.UseInvisibilityCriterion
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.getAbilityLevel
import com.williambl.haema.util.HaemaGameRules
import com.williambl.haema.vampireComponent
import dev.onyxstudios.cca.api.v3.component.sync.AutoSyncedComponent
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.nbt.NbtCompound
import net.minecraft.network.PacketByteBuf
import net.minecraft.server.network.ServerPlayerEntity
import kotlin.properties.Delegates
import kotlin.reflect.KProperty

class EntityStrengthAbilityComponent(val entity: LivingEntity): StrengthAbilityComponent {
    override fun serverTick() {
        val blood = entity.vampireComponent.blood
        if (blood >= 10 && entity.getAbilityLevel(AbilityModule.STRENGTH) > 0) {
            entity.addStatusEffect(
                StatusEffectInstance(
                    VampiricStrengthEffect.instance, 40, when {
                        blood >= 19 -> 2
                        blood >= 14 -> 1
                        else -> 0
                    }.coerceAtMost((entity).getAbilityLevel(AbilityModule.STRENGTH)-1), false, false, true)
            )
        }
    }
    override fun writeToNbt(tag: NbtCompound) {
    }

    override fun readFromNbt(tag: NbtCompound) {
    }
}
