package com.williambl.haema.entity

import com.williambl.haema.Vampirable
import net.minecraft.enchantment.EnchantmentHelper
import net.minecraft.enchantment.Enchantments
import net.minecraft.entity.*
import net.minecraft.entity.ai.goal.*
import net.minecraft.entity.data.DataTracker
import net.minecraft.entity.data.TrackedData
import net.minecraft.entity.data.TrackedDataHandlerRegistry
import net.minecraft.entity.mob.MobEntity
import net.minecraft.entity.passive.AbstractTraderEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.projectile.ProjectileEntity
import net.minecraft.entity.raid.RaiderEntity
import net.minecraft.inventory.SimpleInventory
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.nbt.CompoundTag
import net.minecraft.nbt.ListTag
import net.minecraft.sound.SoundEvent
import net.minecraft.sound.SoundEvents
import net.minecraft.world.LocalDifficulty
import net.minecraft.world.World
import net.minecraft.world.WorldAccess

class VampireHunterEntity(entityType: EntityType<out RaiderEntity>?, world: World?) : RaiderEntity(entityType, world), CrossbowUser {
    private val inventory = SimpleInventory(5)

    override fun initialize(world: WorldAccess?, difficulty: LocalDifficulty?, spawnReason: SpawnReason?, entityData: EntityData?, entityTag: CompoundTag?): EntityData? {
        initEquipment(difficulty)
        return super.initialize(world, difficulty, spawnReason, entityData, entityTag)
    }

    override fun initGoals() {
        super.initGoals()
        goalSelector.add(0, SwimGoal(this))
        goalSelector.add(3, CrossbowAttackGoal(this, 1.0, 8.0f))
        goalSelector.add(8, WanderAroundGoal(this, 0.6))
        goalSelector.add(9, LookAtEntityGoal(this, PlayerEntity::class.java, 15.0f, 1.0f))
        goalSelector.add(10, LookAtEntityGoal(this, MobEntity::class.java, 15.0f))

        targetSelector.add(1, RevengeGoal(this, AbstractTraderEntity::class.java).setGroupRevenge())
        targetSelector.add(2, FollowTargetGoal(this, PlayerEntity::class.java, 10, true, false) { it is Vampirable && it.isVampire })
    }

    override fun initDataTracker() {
        super.initDataTracker()
        dataTracker.startTracking(CHARGING, false)
    }

    override fun initEquipment(difficulty: LocalDifficulty?) {
        val crossbow = ItemStack(Items.CROSSBOW)
        val sword = ItemStack(Items.WOODEN_SWORD)
        EnchantmentHelper.set(mapOf(Pair(Enchantments.SMITE, 2)), sword)
        if (random.nextInt(300) == 0) {
            val map = mapOf(Pair(Enchantments.PIERCING, 1))
            EnchantmentHelper.set(map, crossbow)
        }
        equip(300, crossbow)
        equip(301, sword)
    }

    override fun equip(slot: Int, item: ItemStack?): Boolean {
        return if (super.equip(slot, item)) {
            true
        } else {
            val i = slot - 300
            if (i >= 0 && i < inventory.size()) {
                inventory.setStack(i, item)
                true
            } else {
                false
            }
        }
    }

    override fun writeCustomDataToTag(tag: CompoundTag) {
        super.writeCustomDataToTag(tag)
        val listTag = ListTag()
        for (i in 0 until inventory.size()) {
            val itemStack = inventory.getStack(i)
            if (!itemStack.isEmpty) {
                listTag.add(itemStack.toTag(CompoundTag()))
            }
        }
        tag.put("Inventory", listTag)
    }

    override fun readCustomDataFromTag(tag: CompoundTag) {
        super.readCustomDataFromTag(tag)
        val listTag = tag.getList("Inventory", 10)
        for (i in listTag.indices) {
            val itemStack = ItemStack.fromTag(listTag.getCompound(i))
            if (!itemStack.isEmpty) {
                inventory.addStack(itemStack)
            }
        }
        setCanPickUpLoot(true)
    }

    override fun getCelebratingSound(): SoundEvent = SoundEvents.ENTITY_VILLAGER_CELEBRATE

    override fun addBonusForWave(wave: Int, unused: Boolean) {}

    override fun attack(target: LivingEntity?, pullProgress: Float) {
        shoot(this, 1.6f)
    }

    override fun postShoot() {}

    override fun setCharging(charging: Boolean) {
        dataTracker[CHARGING] = true
    }

    fun isCharging(): Boolean = dataTracker[CHARGING]

    override fun shoot(
        target: LivingEntity?,
        crossbow: ItemStack?,
        projectile: ProjectileEntity?,
        multiShotSpray: Float
    ) {
        shoot(this, target, projectile, multiShotSpray, 1.6f)
    }

    companion object {
        val CHARGING: TrackedData<Boolean> = DataTracker.registerData(VampireHunterEntity::class.java, TrackedDataHandlerRegistry.BOOLEAN)
    }
}

