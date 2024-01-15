package com.williambl.haema.hunter

import com.williambl.haema.id
import com.williambl.haema.isVampire
import com.williambl.haema.util.contains
import net.minecraft.block.entity.BannerPattern
import net.minecraft.block.entity.BannerPatterns
import net.minecraft.command.argument.EntityAnchorArgumentType
import net.minecraft.enchantment.EnchantmentHelper
import net.minecraft.enchantment.Enchantments
import net.minecraft.entity.*
import net.minecraft.entity.ai.goal.*
import net.minecraft.entity.damage.DamageSource
import net.minecraft.entity.data.DataTracker
import net.minecraft.entity.data.TrackedData
import net.minecraft.entity.data.TrackedDataHandlerRegistry
import net.minecraft.entity.mob.MobEntity
import net.minecraft.entity.mob.PathAwareEntity
import net.minecraft.entity.mob.PatrolEntity
import net.minecraft.entity.passive.MerchantEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.projectile.ProjectileEntity
import net.minecraft.entity.projectile.ProjectileUtil
import net.minecraft.inventory.SimpleInventory
import net.minecraft.item.*
import net.minecraft.loot.context.LootContextParameterSet
import net.minecraft.loot.context.LootContextParameters
import net.minecraft.loot.context.LootContextTypes
import net.minecraft.nbt.NbtCompound
import net.minecraft.nbt.NbtList
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions
import net.minecraft.registry.tag.ItemTags
import net.minecraft.server.world.ServerWorld
import net.minecraft.text.Text
import net.minecraft.util.ActionResult
import net.minecraft.util.DyeColor
import net.minecraft.util.Formatting
import net.minecraft.util.Hand
import net.minecraft.util.math.random.Random
import net.minecraft.world.Difficulty
import net.minecraft.world.LocalDifficulty
import net.minecraft.world.ServerWorldAccess
import net.minecraft.world.World

class VampireHunterEntity(entityType: EntityType<out VampireHunterEntity>?, world: World?) : PatrolEntity(
    entityType,
    world
), CrossbowUser {
    private var hasGivenContract = false
    val inventory = SimpleInventory(5)

    override fun initialize(
        world: ServerWorldAccess?,
        difficulty: LocalDifficulty,
        spawnReason: SpawnReason?,
        entityData: EntityData?,
        entityTag: NbtCompound?
    ): EntityData? {
        val result =  super.initialize(world, difficulty, spawnReason, entityData, entityTag)
        if (spawnReason == SpawnReason.SPAWN_EGG && random.nextDouble() < 0.3) {
            isPatrolLeader = true
        }
        initEquipment(this.random, difficulty)
        return result
    }

    override fun initGoals() {
        super.initGoals()
        goalSelector.add(0, SwimGoal(this))
        goalSelector.add(1, VampireHunterOnHorseAttackGoal(this, 1.3, 10.0f))
        goalSelector.add(2, VampireHunterCrossbowAttackGoal(this, 1.0, 8.0f))
        goalSelector.add(3, VampireHunterMeleeAttackGoal(this, 1.0, false))
        goalSelector.add(8, WanderAroundGoal(this, 0.6))
        goalSelector.add(9, LookAtEntityGoal(this, PlayerEntity::class.java, 15.0f, 1.0f))
        goalSelector.add(10, LookAtEntityGoal(this, MobEntity::class.java, 15.0f))

        targetSelector.add(1, RevengeGoal(this, MerchantEntity::class.java).setGroupRevenge())
        targetSelector.add(2, ActiveTargetGoal(this, LivingEntity::class.java, 10, true, false, LivingEntity::isVampire))
    }

    override fun initDataTracker() {
        super.initDataTracker()
        dataTracker.startTracking(CHARGING, false)
    }

    override fun initEquipment(random: Random, difficulty: LocalDifficulty) {
        val crossbow = ItemStack(Items.CROSSBOW)

        val crossbowEnchants = mutableMapOf(Pair(Enchantments.QUICK_CHARGE, 3))
        if (random.nextInt(300) == 0) {
            crossbowEnchants[Enchantments.PIERCING] = 1
        }
        EnchantmentHelper.set(crossbowEnchants, crossbow)
        inventory.addStack(crossbow)

        val sword = ItemStack(if (random.nextDouble() + 1.0 > difficulty.localDifficulty) Items.IRON_SWORD else Items.WOODEN_SWORD)
        sword.addEnchantment(Enchantments.SMITE, 2)
        inventory.addStack(sword)

        if (isPatrolLeader) {
            val banner = ItemStack(Items.WHITE_BANNER)
            val compoundNbt: NbtCompound = banner.getOrCreateSubNbt("BlockEntityTag")
            val listNbt = BannerPattern.Patterns()
                .add(BannerPatterns.RHOMBUS, DyeColor.RED)
                .add(BannerPatterns.HALF_HORIZONTAL_BOTTOM, DyeColor.LIGHT_BLUE)
                .add(BannerPatterns.CIRCLE, DyeColor.RED)
                .toNbt()
            compoundNbt.put("Patterns", listNbt)
            banner.addHideFlag(ItemStack.TooltipSection.ADDITIONAL)
            banner.setCustomName(Text.translatable("block.haema.righteous_banner").formatted(Formatting.GOLD))
            equipStack(EquipmentSlot.HEAD, banner)
        }
    }

    fun takeItem(item: Item): ItemStack {
        return takeItem { it.item == item }
    }

    fun takeItem(predicate: (ItemStack) -> Boolean): ItemStack {
        for (i in 0 until inventory.size()) {
            val stack = inventory.getStack(i)
            if (predicate(stack)) {
                inventory.removeStack(i)
                return stack
            }
        }
        return ItemStack.EMPTY
    }

    override fun writeCustomDataToNbt(tag: NbtCompound) {
        super.writeCustomDataToNbt(tag)
        val listNbt = NbtList()
        for (i in 0 until inventory.size()) {
            val itemStack = inventory.getStack(i)
            if (!itemStack.isEmpty) {
                listNbt.add(itemStack.writeNbt(NbtCompound()))
            }
        }
        tag.put("Inventory", listNbt)
    }

    override fun readCustomDataFromNbt(tag: NbtCompound) {
        super.readCustomDataFromNbt(tag)
        val listNbt = tag.getList("Inventory", 10)
        for (i in listNbt.indices) {
            val itemStack = ItemStack.fromNbt(listNbt.getCompound(i))
            if (!itemStack.isEmpty) {
                inventory.addStack(itemStack)
            }
        }
        setCanPickUpLoot(true)
        hasGivenContract = tag.getBoolean("HasGivenContract")
    }

    override fun mobTick() {
        if (isHolding { it !is ToolItem && it !is CrossbowItem && it != Items.AIR }) {
            if (isHolding { it is VampireHunterContract } ) {
                val stack = mainHandStack
                if (stack.isContractFulfilled()) {
                    world?.server?.lootManager?.getLootTable(paymentLootTable)?.generateLoot(
                        LootContextParameterSet.Builder(world as ServerWorld)
                            .add(LootContextParameters.THIS_ENTITY, this)
                            .build(LootContextTypes.BARTER)
                    )?.forEach { dropStack(it) }
                    equipStack(EquipmentSlot.MAINHAND, ItemStack.EMPTY)
                }
            } else {
                inventory.addStack(mainHandStack)
                equipStack(EquipmentSlot.MAINHAND, ItemStack.EMPTY)
            }
        }
    }

    override fun dropLoot(source: DamageSource?, causedByPlayer: Boolean) {
        super.dropLoot(source, causedByPlayer)
        for (i in 0..inventory.size()) {
            if (random.nextFloat() < 0.1) {
                dropStack(inventory.getStack(i))
            }
        }
        if (random.nextFloat() < 0.1) {
            dropStack(mainHandStack)
        }
    }

    override fun canPickupItem(stack: ItemStack): Boolean {
        return stack.item is VampireHunterContract
    }

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
        shoot(this,
            target,
            if (world.getLocalDifficulty(blockPos).isHarderThan(Difficulty.EASY.id+random.nextFloat()*2)) {
                ProjectileUtil.createArrowProjectile(
                    this,
                    PotionUtil.setPotion(Items.TIPPED_ARROW.defaultStack, Potions.WEAKNESS),
                    1f
                )
            } else {
                projectile
            },
            multiShotSpray, 1.6f)
    }

    override fun isTeammate(other: Entity?): Boolean {
        return if (super.isTeammate(other)) {
            true
        } else if (other is VampireHunterEntity || other is MerchantEntity) {
            this.scoreboardTeam == null && other.scoreboardTeam == null
        } else {
            false
        }
    }

    override fun interactMob(player: PlayerEntity, hand: Hand): ActionResult {
        if (!world.isClient && hand == Hand.MAIN_HAND && this.isPatrolLeader && !((player).isVampire)) {
            val stack = player.mainHandStack
            if (stack.item is VampireHunterContract && stack.isContractFulfilled()) {
                player.inventory.removeOne(stack)
                world?.server?.lootManager?.getLootTable(paymentLootTable)?.generateLoot(
                    LootContextParameterSet.Builder(world as ServerWorld)
                        .add(LootContextParameters.THIS_ENTITY, this)
                        .build(LootContextTypes.BARTER)
                )?.forEach { player.giveItemStack(it) }
            } else if (!hasGivenContract) {
                player.giveItemStack(createContract())
                hasGivenContract = true
            }
            return ActionResult.SUCCESS
        }
        return ActionResult.PASS
    }

    override fun tickRiding() {
        super.tickRiding()
        if (vehicle is PathAwareEntity) {
            bodyYaw = (vehicle as PathAwareEntity).bodyYaw
        }
    }

    fun isHolding(predicate: (Item) -> Boolean): Boolean {
        return predicate.invoke(mainHandStack.item) || predicate.invoke(offHandStack.item)
    }

    private fun createContract(): ItemStack {
        if (random.nextDouble() < 0.3) {
            val target = world.players.filter { (it).isVampire }.randomOrNull()
            if (target != null) {
                return VampireHunterModule.VAMPIRE_HUNTER_CONTRACT.defaultStack.also { it.setContractTarget(target) }
            }
        }
        return VampireHunterModule.VAMPIRE_HUNTER_CONTRACT.defaultStack
    }

    companion object {
        val CHARGING: TrackedData<Boolean> = DataTracker.registerData(
            VampireHunterEntity::class.java,
            TrackedDataHandlerRegistry.BOOLEAN
        )
        val paymentLootTable = id("gameplay/contract_payment")
    }
}

open class VampireHunterCrossbowAttackGoal(private val actor: VampireHunterEntity, speed: Double, range: Float) : CrossbowAttackGoal<VampireHunterEntity>(
    actor,
    speed,
    range
) {
    override fun canStart(): Boolean = hasValidTarget() && actorHasCrossbow()

    override fun shouldContinue(): Boolean =
        hasValidTarget() && (canStart() || !this.actor.navigation.isIdle) && actorHasCrossbow()

    protected open fun hasValidTarget(): Boolean = actor.target != null && actor.target!!.isAlive && actor.squaredDistanceTo(
        actor.target!!
    ) > 25 && actor.target!!.health > 4

    private fun actorHasCrossbow(): Boolean =
        actor.isHolding(Items.CROSSBOW) || actor.inventory.containsAny(setOf(Items.CROSSBOW))

    override fun start() {
        super.start()
        if (!actor.isHolding(Items.CROSSBOW)) {
            val equipped = actor.getStackInHand(Hand.MAIN_HAND)
            actor.equipStack(EquipmentSlot.MAINHAND, actor.takeItem(Items.CROSSBOW))
            actor.inventory.addStack(equipped)
        }
    }

    override fun stop() {
        super.stop()
        if (actor.isHolding(Items.CROSSBOW)) {
            val equipped = actor.getStackInHand(Hand.MAIN_HAND)
            if (actor.inventory.canInsert(equipped)) {
                actor.equipStack(EquipmentSlot.MAINHAND, ItemStack.EMPTY)
                actor.inventory.addStack(equipped)
            }
        }
    }
}

class VampireHunterMeleeAttackGoal(private val actor: VampireHunterEntity, speed: Double, pauseWhenMobIdle: Boolean) : MeleeAttackGoal(
    actor,
    speed,
    pauseWhenMobIdle
) {
    override fun canStart(): Boolean = super.canStart() && hasValidTarget() && actorHasSword()

    override fun shouldContinue(): Boolean = super.shouldContinue() && hasValidTarget() && actorHasSword()

    private fun hasValidTarget(): Boolean = actor.target != null && actor.target!!.isAlive

    private fun actorHasSword(): Boolean =
        actor.isHolding(::isSword) || actor.inventory.contains(::isSword)

    override fun start() {
        super.start()
        if (!actor.isHolding(::isSword)) {
            val equipped = actor.getStackInHand(Hand.MAIN_HAND)
            actor.equipStack(EquipmentSlot.MAINHAND, actor.takeItem(::isSword))
            actor.inventory.addStack(equipped)
        }
    }

    override fun tick() {
        if (actor.target == null) return
        super.tick()
    }

    override fun stop() {
        super.stop()
        if (!actor.isHolding(::isSword)) {
            val equipped = actor.getStackInHand(Hand.MAIN_HAND)
            if (actor.inventory.canInsert(equipped)) {
                actor.equipStack(EquipmentSlot.MAINHAND, ItemStack.EMPTY)
                actor.inventory.addStack(equipped)
            }
        }
    }

    override fun getSquaredMaxAttackDistance(entity: LivingEntity): Double {
        var value = super.getSquaredMaxAttackDistance(entity)
        if (entity.vehicle is LivingEntity) {
            value *= 5
        }
        return value
    }

    private fun isSword(item: ItemStack) = item.isIn(ItemTags.SWORDS)
}

class VampireHunterOnHorseAttackGoal(private val actor: VampireHunterEntity, speed: Double, range: Float) : VampireHunterCrossbowAttackGoal(
    actor,
    speed,
    range
) {
    override fun canStart(): Boolean = actor.hasVehicle() && super.canStart()

    override fun shouldContinue(): Boolean =
        actor.hasVehicle() && super.canStart()

    override fun hasValidTarget(): Boolean = actor.target != null && actor.target!!.isAlive

    override fun tick() {
        super.tick()
        val vehicle = actor.vehicle
        if (vehicle is LivingEntity) {
            vehicle.lookAt(EntityAnchorArgumentType.EntityAnchor.EYES, vehicle.eyePos.add(vehicle.velocity).multiply(2.0))
            actor.lookAt(EntityAnchorArgumentType.EntityAnchor.EYES, actor.target?.pos ?: actor.pos)
        }
    }
}