package com.williambl.haema.vampiremobs

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.VampireAbility
import com.williambl.haema.ability.component.dash.DashAbilityComponent
import com.williambl.haema.api.VampireBurningEvents
import com.williambl.haema.effect.SunlightSicknessEffect
import com.williambl.haema.isVampire
import com.williambl.haema.vampireComponent
import net.minecraft.entity.EntityData
import net.minecraft.entity.EntityType
import net.minecraft.entity.SpawnReason
import net.minecraft.entity.ai.goal.*
import net.minecraft.entity.attribute.DefaultAttributeContainer
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.mob.HostileEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.nbt.NbtCompound
import net.minecraft.util.math.Vec3d
import net.minecraft.world.LocalDifficulty
import net.minecraft.world.ServerWorldAccess
import net.minecraft.world.World
import java.util.function.Predicate
import kotlin.math.max
import kotlin.math.min

class VampiragerEntity(entityType: EntityType<out VampiragerEntity>, world: World) : HostileEntity(entityType, world) {
    private var spawnData: VampiragerData? = null
    private lateinit var dashThroughPathGoal: VampireDashThroughPathGoal

    override fun initialize(
        world: ServerWorldAccess,
        difficulty: LocalDifficulty,
        spawnReason: SpawnReason,
        entityData: EntityData?,
        entityNbt: NbtCompound?
    ): EntityData? {
        this.spawnData = entityData.takeIf { it is VampiragerData } as VampiragerData? ?: VampiragerData(mapOf(AbilityModule.DASH to 1), 100)

        return super.initialize(world, difficulty, spawnReason, this.spawnData, entityNbt)
    }

    override fun initGoals() {
        this.dashThroughPathGoal = VampireDashThroughPathGoal(this, 5.0, 16.0)
        this.goalSelector.add(1, this.dashThroughPathGoal)
        this.goalSelector.add(2, MeleeAttackGoal(this, 1.0, true))
        this.goalSelector.add(4, WanderAroundFarGoal(this, 0.25))
        this.goalSelector.add(6, LookAtEntityGoal(this, PlayerEntity::class.java, 6.0f))
        this.goalSelector.add(7, LookAroundGoal(this))
        this.targetSelector.add(1, RevengeGoal(this))
        this.targetSelector.add(2, ActiveTargetGoal(this, PlayerEntity::class.java, 10, true, false) { !it.isVampire })
    }

    override fun tick() {
        super.tick()
        if (!this.world.isClient) {
            this.spawnData?.let {
                this.vampireComponent.abilities.putAll(it.abilities)
                DashAbilityComponent.entityKey.get(this).updateDashCooldown(it.dashCooldown)
                this.spawnData = null
            }
        }
    }

    override fun mobTick() {
        super.mobTick()
        if (
            !this.isDead
            && !this.world.isClient
            && VampireBurningEvents.TRIGGER.invoker().willVampireBurn(this, this.world).get()
            && VampireBurningEvents.VETO.invoker().willVampireBurn(this, this.world).get()
        ) {
            this.addStatusEffect(StatusEffectInstance(SunlightSicknessEffect.instance, 10, 0, false, false, true))
        }
    }

    fun dashTarget(): Vec3d? {
        return this.dashThroughPathGoal.dashTarget
    }

    companion object {
        fun createVampiragerAttributes(): DefaultAttributeContainer.Builder {
            return createHostileAttributes()
                .add(EntityAttributes.GENERIC_MAX_HEALTH, 30.0)
                .add(EntityAttributes.GENERIC_MOVEMENT_SPEED, 0.25)
                .add(EntityAttributes.GENERIC_FOLLOW_RANGE, 32.0)
        }
    }

    data class VampiragerData(val abilities: Map<VampireAbility, Int>, val dashCooldown: Int) : EntityData
}