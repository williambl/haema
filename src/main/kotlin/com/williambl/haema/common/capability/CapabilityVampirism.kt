package com.williambl.haema.common.capability

import net.minecraft.nbt.NBTBase
import net.minecraft.nbt.NBTPrimitive
import net.minecraft.nbt.NBTTagFloat
import net.minecraft.util.EnumFacing
import net.minecraftforge.common.capabilities.Capability

interface ICapabilityVampirism {
    fun getBloodthirst(): Float
    fun setBloodthirst(input: Float)
    fun addBloodthirst(input: Float)
}

class CapabilityVampirismImpl: ICapabilityVampirism {

    private var bloodthirst: Float = 0.0f

    override fun getBloodthirst(): Float {
        return bloodthirst
    }

    override fun setBloodthirst(input: Float) {
        bloodthirst = input
        if (bloodthirst < 0.0f) bloodthirst = 0.0f
    }

    override fun addBloodthirst(input: Float) {
        bloodthirst += input
        if (bloodthirst < 0.0f) bloodthirst = 0.0f
    }
}

class VampirismStorage: Capability.IStorage<ICapabilityVampirism> {

    override fun readNBT(capability: Capability<ICapabilityVampirism>?, instance: ICapabilityVampirism?, side: EnumFacing?, nbt: NBTBase?) {
        instance?.setBloodthirst((nbt as NBTPrimitive).float)
    }

    override fun writeNBT(capability: Capability<ICapabilityVampirism>?, instance: ICapabilityVampirism?, side: EnumFacing?): NBTBase? {
        return instance?.getBloodthirst()?.let { NBTTagFloat(it) }
    }

}