package com.williambl.haema.common.util

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
    val cap = this.getCapability(VampirismProvider.vampirism!!, null)!!
    cap.addBloodLevel(amount)
    ModPackets.instance.sendTo(SyncVampirismMessage(cap), this as EntityPlayerMP)
    if (amount > 0.0f)
        this.giveVampiricStrength(200, ceil(10.0f * amount.pow(2)).toInt())
}

fun EntityPlayer.giveVampiricStrength(duration: Int, amplifier: Int) {
    this.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_strength")!!, duration, amplifier))
}

fun EntityPlayer.giveVampiricWeakness(duration: Int, amplifier: Int) {
    this.addPotionEffect(PotionEffect(Potion.getPotionFromResourceLocation("haema:vampiric_weakness")!!, duration, amplifier))
}
