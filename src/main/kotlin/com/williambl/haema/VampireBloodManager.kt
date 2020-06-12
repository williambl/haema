package com.williambl.haema

import com.jamieswhiteshirt.reachentityattributes.ReachEntityAttributes
import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled
import net.fabricmc.fabric.api.network.ServerSidePacketRegistry
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.attribute.EntityAttributeModifier
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.player.HungerManager
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundTag
import net.minecraft.network.PacketByteBuf
import net.minecraft.world.GameRules
import java.util.*
import kotlin.math.max
import kotlin.math.min
import kotlin.math.pow
import kotlin.math.roundToInt

class VampireBloodManager : HungerManager() {

    companion object {
        private val VAMPIRE_REACH_UUID = UUID.fromString("0eb4fc5f-71d5-4440-b517-bcc18e1df6f4")
        private val VAMPIRE_ATTACK_RANGE_UUID = UUID.fromString("3267a46b-2b48-429f-a3a8-439aa87a876d")
        private val VAMPIRE_REACH = EntityAttributeModifier(VAMPIRE_REACH_UUID, "Vampire reach extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
        private val VAMPIRE_ATTACK_RANGE = EntityAttributeModifier(VAMPIRE_ATTACK_RANGE_UUID, "Vampire attack range extension", 2.0, EntityAttributeModifier.Operation.ADDITION)
    }

    //TODO: make this have effect
    var canSprint = false

    @Deprecated("use getBloodLevel()")
    var absoluteBloodLevel: Double = 0.0

    override fun update(player: PlayerEntity?) {
        player!!

        canSprint = getBloodLevel() >= 6
        player.isSilent = (getBloodLevel() >= 10 && player.isSprinting) || getBloodLevel() >= 12

        if (getBloodLevel() <= 3) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.WEAKNESS, 1, 4 - getBloodLevel().roundToInt()))
        }

        val reachAttr = player.getAttributeInstance(ReachEntityAttributes.REACH)
        val attackRangeAttr = player.getAttributeInstance(ReachEntityAttributes.ATTACK_RANGE)

        if (getBloodLevel() >= 6 && (reachAttr?.hasModifier(VAMPIRE_REACH) == false || attackRangeAttr?.hasModifier(
                VAMPIRE_ATTACK_RANGE
            ) == false)
        ) {
            reachAttr?.addTemporaryModifier(VAMPIRE_REACH)
            attackRangeAttr?.addTemporaryModifier(VAMPIRE_ATTACK_RANGE)
        } else if (reachAttr?.hasModifier(VAMPIRE_REACH) != false || attackRangeAttr?.hasModifier(VAMPIRE_ATTACK_RANGE) != false) {
            reachAttr?.removeModifier(VAMPIRE_REACH)
            attackRangeAttr?.removeModifier(VAMPIRE_ATTACK_RANGE)
        }

        if (getBloodLevel() >= 8) {
            if (player.world.gameRules.get(GameRules.NATURAL_REGENERATION).get() && player.canFoodHeal()) {
                player.heal(1.0f)
            }
        }

        if (getBloodLevel() >= 10) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.STRENGTH, 1, 1))
            //TODO: faster sprinting
        }

        if (getBloodLevel() >= 12) {
            //TODO: shader
        }

        if (getBloodLevel() >= 14) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.STRENGTH, 1, 2))
        }

        if (getBloodLevel() >= 15) {
            //TODO: shader part 2 electric boogaloo
        }

        if (getBloodLevel() >= 18) {
            //TODO: dash/flight like yoshi but fast
        }

        if (getBloodLevel() >= 19) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.STRENGTH, 1, 3))
        }

        if (getBloodLevel() >= 20) {
            player.addStatusEffect(StatusEffectInstance(StatusEffects.INVISIBILITY, 1, 1))
        }

        sync(player)
    }

    @Deprecated("use blood")
    override fun add(food: Int, f: Float) {}
    @Deprecated("use blood")
    override fun addExhaustion(exhaustion: Float) {}
    @Deprecated("use blood")
    override fun eat(item: Item?, stack: ItemStack?) {}
    @Deprecated("use blood")
    override fun setFoodLevel(foodLevel: Int) {}
    @Deprecated("use blood", ReplaceWith("getBloodLevel().roundToInt()", "kotlin.math.roundToInt"))
    override fun getFoodLevel(): Int {
        return getBloodLevel().roundToInt()
    }

    override fun fromTag(tag: CompoundTag?) {
        super.fromTag(tag)
        absoluteBloodLevel = tag?.getDouble("BloodLevel") ?: 0.0
    }

    override fun toTag(tag: CompoundTag?) {
        super.toTag(tag)
        tag?.putDouble("BloodLevel", absoluteBloodLevel)
    }

    fun getBloodLevel(): Double {
        return 20.0 * ( 1.0 - (1.0 - absoluteBloodLevel /20.0).pow(2) )
    }

    fun removeBlood(blood: Double) {
        absoluteBloodLevel = max(absoluteBloodLevel - blood, 0.0)
    }

    fun addBlood(blood: Double) {
        absoluteBloodLevel = min(absoluteBloodLevel + blood, 20.0)
    }

    fun feed(entity: LivingEntity) {
        //TODO
    }

    private fun sync(player: PlayerEntity) {
        val buf = PacketByteBuf(Unpooled.buffer())
        buf.writeDouble(absoluteBloodLevel)
        ServerSidePacketRegistry.INSTANCE.sendToPlayer(player, bloodLevelPackeId, buf)
    }
}
