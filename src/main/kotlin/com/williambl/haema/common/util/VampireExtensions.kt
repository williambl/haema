package com.williambl.haema.common.util

import com.williambl.haema.common.capability.ICapabilityVampirism
import com.williambl.haema.common.networking.ModPackets
import com.williambl.haema.common.networking.SyncVampirismMessage
import com.williambl.haema.objectholder.ModCapabilityHolder
import com.williambl.haema.objectholder.ModEffectHolder
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.player.ServerPlayerEntity
import net.minecraft.potion.Effect
import net.minecraft.potion.EffectInstance
import net.minecraft.util.math.BlockPos
import net.minecraftforge.common.util.LazyOptional
import net.minecraftforge.fml.network.PacketDistributor
import kotlin.math.ceil
import kotlin.math.pow

fun PlayerEntity.addBlood(amount: Float) {
    this.getVampirismCapability().ifPresent {
        it.addBloodLevel(amount)
        this.syncVampirismCapability(it)
        if (amount > 0.0f)
            this.giveVampiricStrength(200, ceil(10.0f * amount.pow(2)).toInt())
    }
}

fun PlayerEntity.isVampire(): Boolean {
    var value = false
    this.getCapability(ModCapabilityHolder.vampirism, null).ifPresent { value = it.isVampire() }
    return value
}

fun PlayerEntity.syncVampirismCapability(cap: ICapabilityVampirism) {
    ModPackets.instance.send(PacketDistributor.PLAYER.with { this as ServerPlayerEntity }, SyncVampirismMessage(cap))
}

fun PlayerEntity.syncVampirismCapability() {
    ModPackets.instance.send(PacketDistributor.PLAYER.with { this as ServerPlayerEntity }, SyncVampirismMessage(this.getVampirismCapabilityOrThrow()))
}

fun PlayerEntity.giveVampiricStrength(duration: Int, amplifier: Int) {
    this.addPotionEffect(EffectInstance(ModEffectHolder.vampiric_strength, duration, amplifier))
}

fun PlayerEntity.giveVampiricWeakness(duration: Int, amplifier: Int) {
    this.addPotionEffect(EffectInstance(ModEffectHolder.vampiric_weakness, duration, amplifier))
}

fun PlayerEntity.hasEffect(effect: Effect): Boolean {
    return this.getActivePotionEffect(effect) != null
}

fun PlayerEntity.getVampirismCapabilityOrThrow(): ICapabilityVampirism {
    return this.getCapability(ModCapabilityHolder.vampirism, null).orElseThrow(::NullPointerException)
}

fun PlayerEntity.getVampirismCapability(): LazyOptional<ICapabilityVampirism> {
    return this.getCapability(ModCapabilityHolder.vampirism, null)
}

fun PlayerEntity.hasVampirismCapability(): Boolean {
    return this.getCapability(ModCapabilityHolder.vampirism, null).isPresent
}

fun PlayerEntity.isInSunlight(): Boolean {
    return this.world.isDaytime && this.world.canBlockSeeSky(BlockPos(this.posX, this.posY + this.eyeHeight, this.posZ))
}