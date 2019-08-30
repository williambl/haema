package com.williambl.haema.common.util

import com.williambl.haema.common.capability.ICapabilityVampirism
import com.williambl.haema.common.capability.VampirismProvider
import com.williambl.haema.common.networking.ModPackets
import com.williambl.haema.common.networking.SyncVampirismMessage
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.potion.Potion
import net.minecraft.potion.PotionEffect
import kotlin.math.ceil
import kotlin.math.pow

fun EntityPlayer.addBlood(amount: Float) {
    val cap = this.getVampirismCapability()
    cap.addBloodLevel(amount)
    this.syncVampirismCapability(cap)
    if (amount > 0.0f)
        this.giveVampiricStrength(200, ceil(10.0f * amount.pow(2)).toInt())
}

fun EntityPlayer.isVampire(): Boolean {
    return this.getCapability(VampirismProvider.vampirism!!, null)?.isVampire() ?: false
}

fun EntityPlayer.syncVampirismCapability(cap: ICapabilityVampirism) {
    ModPackets.instance.sendTo(SyncVampirismMessage(cap), this as EntityPlayerMP)
}

fun EntityPlayer.syncVampirismCapability() {
    ModPackets.instance.sendTo(SyncVampirismMessage(this.getVampirismCapability()), this as EntityPlayerMP)
}

fun EntityPlayer.giveVampiricStrength(duration: Int, amplifier: Int) {
    this.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_strength")!!, duration, amplifier))
}

fun EntityPlayer.giveVampiricWeakness(duration: Int, amplifier: Int) {
    this.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!, duration, amplifier))
}

fun EntityPlayer.hasPotionEffect(potion: Potion): Boolean {
    return this.getActivePotionEffect(potion) != null
}

fun EntityPlayer.getVampirismCapability(): ICapabilityVampirism {
    return this.getCapability(VampirismProvider.vampirism!!, null)!!
}

fun EntityPlayer.hasVampirismCapability(): Boolean {
    return this.hasCapability(VampirismProvider.vampirism!!, null)
}
