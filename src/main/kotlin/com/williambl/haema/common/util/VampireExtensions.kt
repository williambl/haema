package com.williambl.haema.common.util

import com.williambl.haema.common.capability.ICapabilityVampirism
import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.networking.ModPackets
import com.williambl.haema.common.networking.SyncVampirismMessage
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.player.ServerPlayerEntity
import net.minecraft.potion.Effect
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
    this.getCapability(VampirismProvider.vampirism!!, null).ifPresent { value = it.isVampire() }
    return value
}

fun PlayerEntity.syncVampirismCapability(cap: ICapabilityVampirism) {
    ModPackets.instance.send(PacketDistributor.PLAYER.with { this as ServerPlayerEntity }, SyncVampirismMessage(cap))
}

fun PlayerEntity.syncVampirismCapability() {
    ModPackets.instance.send(PacketDistributor.PLAYER.with { this as ServerPlayerEntity }, SyncVampirismMessage(this.getVampirismCapability()))
}

fun PlayerEntity.giveVampiricStrength(duration: Int, amplifier: Int) {
    this.addPotionEffect(, duration, amplifier))
}

fun PlayerEntity.giveVampiricWeakness(duration: Int, amplifier: Int) {
    this.addPotionEffect(, duration, amplifier))
}

fun PlayerEntity.hasEffect(effect: Effect): Boolean {
    return this.getActivePotionEffect(effect) != null
}

fun PlayerEntity.getVampirismCapability(): LazyOptional<ICapabilityVampirism> {
    return this.getCapability(VampirismProvider.vampirism!!, null)
}

fun PlayerEntity.hasVampirismCapability(): Boolean {
    return this.getCapability(VampirismProvider.vampirism!!, null).isPresent
}

fun PlayerEntity.isInSunlight(): Boolean {
    return this.world.isDaytime && this.world.canBlockSeeSky(BlockPos(this.posX, this.posY + this.eyeHeight, this.posZ))
}