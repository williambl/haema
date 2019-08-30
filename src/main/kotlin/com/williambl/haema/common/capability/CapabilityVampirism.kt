package com.williambl.haema.common.capability

import com.williambl.haema.common.util.VampireAbilities
import net.minecraft.nbt.NBTBase
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumFacing
import net.minecraftforge.common.capabilities.Capability
import net.minecraftforge.common.capabilities.CapabilityInject
import net.minecraftforge.common.capabilities.ICapabilitySerializable
import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow

interface ICapabilityVampirism {
    fun getBloodLevel(): Float
    fun setBloodLevel(input: Float)
    fun addBloodLevel(input: Float)

    fun isVampire(): Boolean
    fun setIsVampire(input: Boolean)

    fun getPowerMultiplier(): Float
    fun getInversePowerMultiplier(): Float

    fun getAbilities(): Int
}

class CapabilityVampirismImpl: ICapabilityVampirism {

    private var bloodLevel: Float = 0.0f
    private var vampire: Boolean = false

    override fun getBloodLevel(): Float {
        return bloodLevel
    }

    override fun setBloodLevel(input: Float) {
        bloodLevel = input
        bloodLevel = min(bloodLevel, 1.0f)
        bloodLevel = max(bloodLevel, 0.0f)
    }

    override fun addBloodLevel(input: Float) {
        bloodLevel += input
        bloodLevel = min(bloodLevel, 1.0f)
        bloodLevel = max(bloodLevel, 0.0f)
    }

    override fun isVampire(): Boolean {
        return vampire
    }

    override fun setIsVampire(input: Boolean) {
        vampire = input
    }

    override fun getPowerMultiplier(): Float {
        return 10.0f * bloodLevel.pow(2)
    }

    override fun getInversePowerMultiplier(): Float {
        return if (bloodLevel > 0.1f)
            (0.1f / bloodLevel.pow(2))
        else
            10.0f
    }

    override fun getAbilities(): Int {
        var value = 0
        when {
            bloodLevel < 0.1 -> value = value or VampireAbilities.WEAKNESS.flag
            bloodLevel > 0.5 -> value = value or VampireAbilities.STRENGTH.flag
            bloodLevel > 0.6 -> value = value or VampireAbilities.VISION.flag
            bloodLevel > 0.75 -> value = value or VampireAbilities.FLIGHT.flag
            bloodLevel > 0.95 -> value = value or VampireAbilities.INVISIBILITY.flag
        }

        return value
    }

}

class VampirismStorage: Capability.IStorage<ICapabilityVampirism> {

    override fun readNBT(capability: Capability<ICapabilityVampirism>?, instance: ICapabilityVampirism?, side: EnumFacing?, nbt: NBTBase?) {
        nbt as NBTTagCompound
        instance?.setBloodLevel(nbt.getFloat("bloodLevel"))
        instance?.setIsVampire(nbt.getBoolean("isVampire"))
    }

    override fun writeNBT(capability: Capability<ICapabilityVampirism>?, instance: ICapabilityVampirism?, side: EnumFacing?): NBTBase? {
        instance ?: return null

        val compound = NBTTagCompound()
        compound.setFloat("bloodLevel", instance.getBloodLevel())
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