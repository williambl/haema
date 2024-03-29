package com.williambl.haema.vampiremobs

import com.williambl.haema.api.VampireBurningEvents.TRIGGER
import com.williambl.haema.api.VampireBurningEvents.VETO
import com.williambl.haema.effect.SunlightSicknessEffect.Companion.instance
import com.williambl.haema.vampireComponent
import net.minecraft.entity.Entity
import net.minecraft.entity.EntityType
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.goal.*
import net.minecraft.entity.damage.DamageSource
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.mob.ZombieEntity
import net.minecraft.entity.passive.IronGolemEntity
import net.minecraft.entity.passive.MerchantEntity
import net.minecraft.entity.passive.TurtleEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NbtCompound
import net.minecraft.nbt.NbtHelper
import net.minecraft.server.world.ServerWorld
import net.minecraft.sound.SoundEvent
import net.minecraft.sound.SoundEvents
import net.minecraft.world.World
import java.util.*

class VampiricZombieEntity(entityType: EntityType<out VampiricZombieEntity>, world: World) : ZombieEntity(entityType, world), OwnedMob {
    var ownerUuid: UUID? = null
        private set

    override var owner: LivingEntity?
        get() {
            return if (ownerUuid == null) {
                null
            } else if (this.world is ServerWorld) {
                val res = (this.world as ServerWorld).getEntity(ownerUuid)
                if (res is LivingEntity && res != this && res.isAlive) {
                    res
                } else {
                    null
                }
            } else {
                null
            }
        }
        set(value) {
            this.ownerUuid = value?.uuid
        }

    override fun initCustomGoals() {
        goalSelector.add(2, ZombieAttackGoal(this, 1.0, true))
        goalSelector.add(6, MoveThroughVillageGoal(this, 1.0, true, 4, ::canBreakDoors))
        goalSelector.add(7, WanderAroundFarGoal(this, 1.0))
        targetSelector.add(1, RevengeGoal(this).setGroupRevenge())
        targetSelector.add(2, TargetOwnersTargetGoal(this))
        targetSelector.add(3, DrinkBloodActiveTargetGoal(this, LivingEntity::class.java, true))
        targetSelector.add(4, ActiveTargetGoal(this, PlayerEntity::class.java, true))
        targetSelector.add(5, ActiveTargetGoal(this, MerchantEntity::class.java, false))
        targetSelector.add(5, ActiveTargetGoal(this, IronGolemEntity::class.java, true))
        targetSelector.add(7, ActiveTargetGoal(this, TurtleEntity::class.java, 10, true, false, TurtleEntity.BABY_TURTLE_ON_LAND_FILTER))
    }

    override fun readCustomDataFromNbt(nbt: NbtCompound) {
        super.readCustomDataFromNbt(nbt)
        ownerUuid = if (nbt.contains(OWNER_NBT_TAG)) {
            NbtHelper.toUuid(nbt.get(OWNER_NBT_TAG))
        } else null
    }

    override fun writeCustomDataToNbt(nbt: NbtCompound) {
        super.writeCustomDataToNbt(nbt)
        ownerUuid?.let { nbt.put(OWNER_NBT_TAG, NbtHelper.fromUuid(it)) }
    }

    override fun canTarget(target: LivingEntity?): Boolean {
        return super.canTarget(target) && target != this.owner && this.owner?.canTarget(target) ?: true
    }

    override fun mobTick() {
        super.mobTick()
        if (
            !isDead
            && !world.isClient
            && TRIGGER.invoker().willVampireBurn(this, world).get()
            && VETO.invoker().willVampireBurn(this, world).get()
        ) {
            this.addStatusEffect(StatusEffectInstance(instance, 10, 0, false, false, true))
        }

        if (!isDead && !world.isClient && owner == null && !this.hasStatusEffect(StatusEffects.WITHER)) {
            this.addStatusEffect(StatusEffectInstance(StatusEffects.WITHER, 10 * 20))
        }
    }

    override fun burnsInDaylight(): Boolean {
        return false // we have our own burning logic
    }

    override fun getAmbientSound(): SoundEvent? {
        return SoundEvents.ENTITY_ZOMBIE_AMBIENT
    }

    override fun getHurtSound(source: DamageSource): SoundEvent? {
        return SoundEvents.ENTITY_ZOMBIE_HURT
    }

    override fun getDeathSound(): SoundEvent? {
        return SoundEvents.ENTITY_ZOMBIE_DEATH
    }

    override fun getStepSound(): SoundEvent? {
        return SoundEvents.ENTITY_ZOMBIE_STEP
    }

    override fun tryAttack(target: Entity?): Boolean {
        val bl = super.tryAttack(target)
        if (bl && target is LivingEntity && this.vampireComponent.blood <= 20) {
            this.vampireComponent.feed(target)
        }
        return bl
    }

    override fun canConvertInWater(): Boolean {
        return false
    }

    override fun getSkull(): ItemStack {
        return ItemStack.EMPTY
    }

    companion object {
        val OWNER_NBT_TAG = "ownerUuid"

        fun convert(zombieEntity: ZombieEntity): VampiricZombieEntity? {
            return zombieEntity.convertTo(VampireMobsModule.VAMPIRIC_ZOMBIE, true)
        }
    }
}