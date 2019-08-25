package com.williambl.haema.common.capability

import net.minecraft.nbt.NBTBase
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumFacing
import net.minecraftforge.common.capabilities.Capability
import net.minecraftforge.common.capabilities.CapabilityInject
import net.minecraftforge.common.capabilities.ICapabilitySerializable
import kotlin.math.pow

interface ICapabilityVampirism {
    fun getBloodthirst(): Float
    fun setBloodthirst(input: Float)
    fun addBloodthirst(input: Float)

    fun isVampire(): Boolean
    fun setIsVampire(input: Boolean)

    fun getPowerMultiplier(): Float
}

class CapabilityVampirismImpl: ICapabilityVampirism {

    private var bloodthirst: Float = 0.0f
    private var vampire: Boolean = false

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

    override fun isVampire(): Boolean {
        return vampire
    }

    override fun setIsVampire(input: Boolean) {
        vampire = input
    }

    override fun getPowerMultiplier(): Float {
        return if (bloodthirst > 0.1f)
            (0.1f / bloodthirst.pow(2))
        else
            10.0f
    }

}

class VampirismStorage: Capability.IStorage<ICapabilityVampirism> {

    override fun readNBT(capability: Capability<ICapabilityVampirism>?, instance: ICapabilityVampirism?, side: EnumFacing?, nbt: NBTBase?) {
        nbt as NBTTagCompound
        instance?.setBloodthirst(nbt.getFloat("bloodthirst"))
        instance?.setIsVampire(nbt.getBoolean("isVampire"))
    }

    override fun writeNBT(capability: Capability<ICapabilityVampirism>?, instance: ICapabilityVampirism?, side: EnumFacing?): NBTBase? {
        instance ?: return null

        val compound = NBTTagCompound()
        compound.setFloat("bloodthirst", instance.getBloodthirst())
        compound.setBoolean("isVampire", instance.isVampire())
        return compound
    }

}

class VampirismProvider : ICapabilitySerializable<NBTBase> {

    companion object {
        @CapabilityInject(ICapabilityVampirism::class)
        val vampirism: Capability<ICapabilityVampirism>? = null
    }

    private val instance: ICapabilityVampirism = vampirism!!.defaultInstance!!

    override fun <T : Any?> getCapability(capability: Capability<T>, facing: EnumFacing?): T? {
        return if (capability == vampirism)
            vampirism.cast(instance)
        else
            null
    }

    override fun deserializeNBT(nbt: NBTBase?) {
        vampirism!!.storage.readNBT(vampirism, instance, null, nbt)
    }

    override fun serializeNBT(): NBTBase {
        return vampirism!!.storage.writeNBT(vampirism, instance, null)!!
    }

    override fun hasCapability(capability: Capability<*>, facing: EnumFacing?): Boolean {
        return capability == vampirism
    }

}