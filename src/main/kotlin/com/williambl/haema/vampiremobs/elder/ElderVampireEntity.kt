package com.williambl.haema.vampiremobs.elder

import com.williambl.haema.ability.AbilityModule
import com.williambl.haema.ability.VampireAbility
import com.williambl.haema.ability.component.dash.DashAbilityComponent
import com.williambl.haema.api.VampireBurningEvents
import com.williambl.haema.effect.SunlightSicknessEffect
import com.williambl.haema.vampireComponent
import com.williambl.haema.vampiremobs.ReinforcementSpawner
import com.williambl.haema.vampiremobs.VampireDashThroughPathGoal
import com.williambl.haema.vampiremobs.VampireMobsModule
import it.unimi.dsi.fastutil.objects.ObjectArrayList
import net.minecraft.entity.*
import net.minecraft.entity.ai.brain.Brain
import net.minecraft.entity.ai.brain.MemoryModuleType
import net.minecraft.entity.attribute.DefaultAttributeContainer
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.mob.HostileEntity
import net.minecraft.entity.mob.MobEntity
import net.minecraft.nbt.NbtCompound
import net.minecraft.server.world.ServerWorld
import net.minecraft.util.math.Vec3d
import net.minecraft.world.LocalDifficulty
import net.minecraft.world.ServerWorldAccess
import net.minecraft.world.World
import net.tslat.smartbrainlib.api.SmartBrainOwner
import net.tslat.smartbrainlib.api.core.BrainActivityGroup
import net.tslat.smartbrainlib.api.core.SmartBrainProvider
import net.tslat.smartbrainlib.api.core.behaviour.FirstApplicableBehaviour
import net.tslat.smartbrainlib.api.core.behaviour.OneRandomBehaviour
import net.tslat.smartbrainlib.api.core.behaviour.custom.attack.AnimatableMeleeAttack
import net.tslat.smartbrainlib.api.core.behaviour.custom.look.LookAtTarget
import net.tslat.smartbrainlib.api.core.behaviour.custom.misc.Idle
import net.tslat.smartbrainlib.api.core.behaviour.custom.move.MoveToWalkTarget
import net.tslat.smartbrainlib.api.core.behaviour.custom.path.SetRandomWalkTarget
import net.tslat.smartbrainlib.api.core.behaviour.custom.path.SetWalkTargetToAttackTarget
import net.tslat.smartbrainlib.api.core.behaviour.custom.target.SetPlayerLookTarget
import net.tslat.smartbrainlib.api.core.behaviour.custom.target.SetRandomLookTarget
import net.tslat.smartbrainlib.api.core.sensor.ExtendedSensor
import net.tslat.smartbrainlib.api.core.sensor.vanilla.HurtBySensor
import net.tslat.smartbrainlib.api.core.sensor.vanilla.NearbyLivingEntitySensor
import java.util.*
import kotlin.jvm.optionals.getOrNull


class ElderVampireEntity(entityType: EntityType<out ElderVampireEntity>, world: World) : HostileEntity(entityType, world),
    ReinforcementSpawner, SmartBrainOwner<ElderVampireEntity> {
    private var spawnData: ElderVampireData? = null
    private lateinit var dashThroughPathGoal: VampireDashThroughPathGoal

    override var timesSpawnedReinforcements: Int = 0
        private set

    override fun initialize(
        world: ServerWorldAccess,
        difficulty: LocalDifficulty,
        spawnReason: SpawnReason,
        entityData: EntityData?,
        entityNbt: NbtCompound?
    ): EntityData? {
        this.spawnData = entityData.takeIf { it is ElderVampireData } as ElderVampireData? ?: ElderVampireData(mapOf(AbilityModule.DASH to 1), 100)

        return super.initialize(world, difficulty, spawnReason, this.spawnData, entityNbt)
    }

    override fun initGoals() {
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

        tickBrain(this)
    }

    override fun spawnReinforcements() {
        for (i in 0..10) {
            val zombie = VampireMobsModule.VAMPIRIC_ZOMBIE.spawn(
                this.world as ServerWorld,
                this.blockPos.add(this.random.nextBetween(-3, 3), 0, this.random.nextBetween(-3, -3)),
                SpawnReason.REINFORCEMENT
            )
            zombie?.owner = this
        }

        this.timesSpawnedReinforcements++
    }

    override fun tryAttack(target: Entity?): Boolean {
        val bl = super.tryAttack(target)
        if (bl && target is LivingEntity && this.vampireComponent.blood <= 20) {
            this.vampireComponent.feed(target)
        }
        return bl
    }

    override fun writeCustomDataToNbt(nbt: NbtCompound) {
        super.writeCustomDataToNbt(nbt)
        nbt.putInt("timesSpawnedReinforcements", this.timesSpawnedReinforcements)
    }

    override fun readCustomDataFromNbt(nbt: NbtCompound) {
        super.readCustomDataFromNbt(nbt)
        this.timesSpawnedReinforcements = nbt.getInt("timesSpawnedReinforcements")
    }

    @OptIn(ExperimentalStdlibApi::class)
    fun dashTarget(): Vec3d? {
        return (this.brain.getOptionalMemory(MemoryModuleType.WALK_TARGET) ?: Optional.empty()).map { t -> t.lookTarget.pos }.filter {
                pos -> (2..400).contains(squaredDistanceTo(pos).toInt())
        }.getOrNull()
    }

    companion object {
        fun createElderVampireAttributes(): DefaultAttributeContainer.Builder {
            return createHostileAttributes()
                .add(EntityAttributes.GENERIC_MAX_HEALTH, 100.0)
                .add(EntityAttributes.GENERIC_MOVEMENT_SPEED, 0.6)
                .add(EntityAttributes.GENERIC_FOLLOW_RANGE, 32.0)
        }
    }

    data class ElderVampireData(val abilities: Map<VampireAbility, Int>, val dashCooldown: Int) : EntityData

    override fun createBrainProfile(): Brain.Profile<ElderVampireEntity> {
        return SmartBrainProvider(this) as Brain.Profile<ElderVampireEntity>
    }
    override fun getSensors(): MutableList<ExtendedSensor<ElderVampireEntity>> {
        return ObjectArrayList.of(
            NearbyLivingEntitySensor(),
            HurtBySensor()
        )
    }

    override fun getCoreTasks(): BrainActivityGroup<ElderVampireEntity> { // These are the tasks that run all the time (usually)
        return BrainActivityGroup.coreTasks(
            LookAtTarget<MobEntity>(),  // Have the entity turn to face and look at its current look target
            DashToWalkTarget(this::dashTarget),
            MoveToWalkTarget()          // Walk towards the current walk target
        )
    }

    override fun getIdleTasks(): BrainActivityGroup<ElderVampireEntity> { // These are the tasks that run when the mob isn't doing anything else (usually)
        return BrainActivityGroup.idleTasks(
            FirstApplicableBehaviour(   // Run only one of the below behaviours, trying each one in order.
                MultiTargetOrRetaliate(),    // Set the attack target and walk target based on nearby entities
                SetPlayerLookTarget(),  // Set the look target for the nearest player
                SetRandomLookTarget()   // Set a random look target
            ),
            SetWalkTargetToAttackTarget<MobEntity>(),  // Set the walk target to the attack target
            OneRandomBehaviour(         // Run a random task from the below options
                SetRandomWalkTarget(),  // Set a random walk target to a nearby position
                Idle<ElderVampireEntity>().runFor { entity -> entity.random.nextInt(57) + 3 } // Do nothing for 1.5->3 seconds
            )
        )
    }

    override fun getFightTasks(): BrainActivityGroup<ElderVampireEntity> { // These are the tasks that handle fighting
        return BrainActivityGroup.fightTasks(
            AnimatableMeleeAttack(0)  // Melee attack the target if close enough
        )
    }
}