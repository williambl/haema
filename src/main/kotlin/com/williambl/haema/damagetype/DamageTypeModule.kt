package com.williambl.haema.damagetype

import com.williambl.haema.id
import net.fabricmc.api.ModInitializer
import net.minecraft.entity.damage.DamageType
import net.minecraft.registry.RegistryKey
import net.minecraft.registry.RegistryKeys

object DamageTypeModule : ModInitializer {
    @JvmField
    val BLOOD_LOSS: RegistryKey<DamageType> = RegistryKey.of(RegistryKeys.DAMAGE_TYPE, id("blood_loss"))

    @JvmField
    val INCOMPATIBLE_BLOOD: RegistryKey<DamageType> = RegistryKey.of(RegistryKeys.DAMAGE_TYPE, id("incompatible_blood"))

    @JvmField
    val SUNLIGHT: RegistryKey<DamageType> = RegistryKey.of(RegistryKeys.DAMAGE_TYPE, id("sunlight"))

    override fun onInitialize() {
    }
}
