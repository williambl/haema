package com.williambl.haema.hunter

import com.williambl.haema.Vampirable
import com.williambl.haema.util.contains
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricDefaultAttributeRegistry
import net.fabricmc.fabric.api.`object`.builder.v1.entity.FabricEntityTypeBuilder
import net.fabricmc.fabric.api.tool.attribute.v1.FabricToolTags
import net.minecraft.block.entity.BannerPattern
import net.minecraft.enchantment.EnchantmentHelper
import net.minecraft.enchantment.Enchantments
import net.minecraft.entity.*
import net.minecraft.entity.ai.goal.*
import net.minecraft.entity.attribute.EntityAttributes
import net.minecraft.entity.data.DataTracker
import net.minecraft.entity.data.TrackedData
import net.minecraft.entity.data.TrackedDataHandlerRegistry
import net.minecraft.entity.mob.HostileEntity
import net.minecraft.entity.mob.MobEntity
import net.minecraft.entity.mob.PathAwareEntity
import net.minecraft.entity.mob.PatrolEntity
import net.minecraft.entity.passive.MerchantEntity
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.entity.projectile.ProjectileEntity
import net.minecraft.entity.projectile.ProjectileUtil
import net.minecraft.inventory.SimpleInventory
import net.minecraft.item.*
import net.minecraft.loot.context.LootContext
import net.minecraft.loot.context.LootContextParameters
import net.minecraft.loot.context.LootContextTypes
import net.minecraft.nbt.CompoundTag
import net.minecraft.nbt.ListTag
import net.minecraft.potion.PotionUtil
import net.minecraft.potion.Potions
import net.minecraft.server.world.ServerWorld
import net.minecraft.text.TranslatableText
import net.minecraft.util.*
import net.minecraft.util.registry.Registry
import net.minecraft.world.Difficulty
import net.minecraft.world.LocalDifficulty
import net.minecraft.world.ServerWorldAccess
import net.minecraft.world.World
import kotlin.Pair

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
        entityTag: CompoundTag?
    ): EntityData? {
        val result =  super.initialize(world, difficulty, spawnReason, entityData, entityTag)
        if (spawnReason == SpawnReason.SPAWN_EGG && random.nextDouble() < 0.3) {
            isPatrolLeader = true
        }
        initEquipment(difficulty)
        return result
    }

    override fun initGoals() {
        super.initGoals()
        goalSelector.add(0, SwimGoal(this))
        goalSelector.add(2, VampireHunterCrossbowAttackGoal(this, 1.0, 8.0f))
        goalSelector.add(3, VampireHunterMeleeAttackGoal(this, 1.0, true))
        goalSelector.add(8, WanderAroundGoal(this, 0.6))
        goalSelector.add(9, LookAtEntityGoal(this, PlayerEntity::class.java, 15.0f, 1.0f))
        goalSelector.add(10, LookAtEntityGoal(this, MobEntity::class.java, 15.0f))

        targetSelector.add(1, RevengeGoal(this, MerchantEntity::class.java).setGroupRevenge())
        targetSelector.add(2, FollowTargetGoal(this, LivingEntity::class.java, 10, true, false) { it is Vampirable && it.isVampire })
    }

    override fun initDataTracker() {
        super.initDataTracker()
        dataTracker.startTracking(CHARGING, false)
    }

    override fun initEquipment(difficulty: LocalDifficulty) {
        val crossbow = ItemStack(Items.CROSSBOW)

        val crossbowEnchants = mutableMapOf(Pair(Enchantments.QUICK_CHARGE, 3))
        if (random.nextInt(300) == 0) {
            crossbowEnchants[Enchantments.PIERCING] = 1
        }
        EnchantmentHelper.set(crossbowEnchants, crossbow)
        equip(300, crossbow)

        val sword = ItemStack(if (random.nextDouble() + 1.0 > difficulty.localDifficulty) Items.IRON_SWORD else Items.WOODEN_SWORD)
        sword.addEnchantment(Enchantments.SMITE, 2)
        equip(301, sword)

        if (isPatrolLeader) {
            val banner = ItemStack(Items.WHITE_BANNER)
            val compoundTag: CompoundTag = banner.getOrCreateSubTag("BlockEntityTag")
            val listTag = BannerPattern.Patterns()
                .add(BannerPattern.RHOMBUS_MIDDLE, DyeColor.RED)
                .add(BannerPattern.HALF_HORIZONTAL_MIRROR, DyeColor.LIGHT_BLUE)
                .add(BannerPattern.CIRCLE_MIDDLE, DyeColor.RED)
                .toTag()
            compoundTag.put("Patterns", listTag)
            @Suppress("UsePropertyAccessSyntax")
            banner.getOrCreateTag().putInt("HideFlags", 32)
            banner.setCustomName(TranslatableText("block.haema.righteous_banner").formatted(Formatting.GOLD))
            equipStack(EquipmentSlot.HEAD, banner)
        }
    }

    override fun equip(slot: Int, item: ItemStack): Boolean {
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

    fun takeItem(item: Item): ItemStack {
        return takeItem { it == item }
    }

    fun takeItem(prediacte: (Item) -> Boolean): ItemStack {
        for (i in 0 until inventory.size()) {
            val stack = inventory.getStack(i)
            if (prediacte(stack.item)) {
                inventory.removeStack(i)
                return stack
            }
        }
        return ItemStack.EMPTY
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
        tag.putBoolean("HasGivenContract", hasGivenContract)
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
        hasGivenContract = tag.getBoolean("HasGivenContract")
    }

    override fun mobTick() {
        if (isHolding { it !is ToolItem && it !is CrossbowItem && it != Items.AIR }) {
            if (isHolding { it is VampireHunterContract } ) {
                val stack = mainHandStack
                if (stack.isContractFulfilled()) {
                    world?.server?.lootManager?.getTable(paymentLootTable)?.generateLoot(
                        LootContext.Builder(world as ServerWorld)
                            .parameter(LootContextParameters.THIS_ENTITY, this).random(world.random)
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
        if (!world.isClient && hand == Hand.MAIN_HAND && this.isPatrolLeader && !((player as Vampirable).isVampire)) {
            val stack = player.mainHandStack
            if (stack.item is VampireHunterContract && stack.isContractFulfilled()) {
                player.inventory.removeOne(stack)
                world?.server?.lootManager?.getTable(paymentLootTable)?.generateLoot(
                    LootContext.Builder(world as ServerWorld)
                        .parameter(LootContextParameters.THIS_ENTITY, this).random(world.random)
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

    private fun createContract(): ItemStack {
        if (random.nextDouble() < 0.3) {
            val target = world.players.filter { (it as Vampirable).isVampire }.randomOrNull()
            if (target != null) {
                return contractItem.defaultStack.also { it.setContractTarget(target) }
            }
        }
        return contractItem.defaultStack
    }

    companion object {
        val CHARGING: TrackedData<Boolean> = DataTracker.registerData(
            VampireHunterEntity::class.java,
            TrackedDataHandlerRegistry.BOOLEAN
        )
        val paymentLootTable = Identifier("haema:gameplay/contract_payment")
        val contractItem: Item by lazy { Registry.ITEM.get(Identifier("haema:vampire_hunter_contract")) }
    }
}

class VampireHunterCrossbowAttackGoal(private val actor: VampireHunterEntity, speed: Double, range: Float) : CrossbowAttackGoal<VampireHunterEntity>(
    actor,
    speed,
    range
) {
    override fun canStart(): Boolean = hasValidTarget() && actorHasCrossbow()

    override fun shouldContinue(): Boolean =
        hasValidTarget() && (canStart() || !this.actor.navigation.isIdle) && actorHasCrossbow()

    private fun hasValidTarget(): Boolean = actor.target != null && actor.target!!.isAlive && actor.squaredDistanceTo(
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

    private fun isSword(item: Item) = FabricToolTags.SWORDS.contains(item)
}

fun registerVampireHunter() {
    Registry.register(
        Registry.ENTITY_TYPE,
        Identifier("haema:vampire_hunter"),
        FabricEntityTypeBuilder.create<VampireHunterEntity>(SpawnGroup.CREATURE) { type, world -> VampireHunterEntity(type, world) }
            .dimensions(EntityDimensions.fixed(0.5f, 2f))
            .trackRangeBlocks(128).trackedUpdateRate(3).spawnableFarFromPlayer().build()
    )

    Registry.register(
        Registry.ITEM,
        Identifier("haema:vampire_hunter_contract"),
        VampireHunterContract(Item.Settings().group(ItemGroup.MISC))
    )

    @Suppress("UNCHECKED_CAST")
    FabricDefaultAttributeRegistry.register(
        Registry.ENTITY_TYPE.get(Identifier("haema:vampire_hunter")) as EntityType<out LivingEntity>?,
        HostileEntity.createHostileAttributes().add(EntityAttributes.GENERIC_MOVEMENT_SPEED, 0.35)
            .add(EntityAttributes.GENERIC_MAX_HEALTH, 20.0)
            .add(EntityAttributes.GENERIC_ATTACK_DAMAGE, 5.0)
            .add(EntityAttributes.GENERIC_FOLLOW_RANGE, 64.0)
    )
}