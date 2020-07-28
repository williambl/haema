package com.williambl.haema

import com.williambl.haema.craft.BookOfBloodRecipe
import com.williambl.haema.damagesource.IncompatibleBloodDamageSource
import com.williambl.haema.effect.SunlightSicknessEffect
import com.williambl.haema.effect.VampiricStrengthEffect
import com.williambl.haema.effect.VampiricWeaknessEffect
import com.williambl.haema.item.EmptyVampireBloodInjectorItem
import com.williambl.haema.item.VampireBloodInjectorItem
import com.williambl.haema.util.addTradesToProfession
import com.williambl.haema.util.raytraceForDash
import net.fabricmc.fabric.api.event.player.UseBlockCallback
import net.fabricmc.fabric.api.event.player.UseEntityCallback
import net.fabricmc.fabric.api.loot.v1.FabricLootPoolBuilder
import net.fabricmc.fabric.api.loot.v1.FabricLootSupplierBuilder
import net.fabricmc.fabric.api.loot.v1.event.LootTableLoadingCallback
import net.fabricmc.fabric.api.loot.v1.event.LootTableLoadingCallback.LootTableSetter
import net.fabricmc.fabric.api.network.ServerSidePacketRegistry
import net.fabricmc.fabric.api.tag.TagRegistry
import net.minecraft.block.BedBlock
import net.minecraft.block.DispenserBlock
import net.minecraft.block.dispenser.FallibleItemDispenserBehavior
import net.minecraft.block.enums.BedPart
import net.minecraft.client.item.TooltipContext
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.effect.StatusEffectInstance
import net.minecraft.entity.effect.StatusEffects
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.Item
import net.minecraft.item.ItemGroup
import net.minecraft.item.ItemStack
import net.minecraft.item.Items
import net.minecraft.loot.ConstantLootTableRange
import net.minecraft.loot.LootManager
import net.minecraft.loot.entry.ItemEntry
import net.minecraft.particle.DustParticleEffect
import net.minecraft.resource.ResourceManager
import net.minecraft.server.world.ServerWorld
import net.minecraft.text.Text
import net.minecraft.text.TranslatableText
import net.minecraft.util.*
import net.minecraft.util.math.BlockPointer
import net.minecraft.util.math.Box
import net.minecraft.util.registry.Registry
import net.minecraft.village.TradeOffer
import net.minecraft.village.TradeOffers
import net.minecraft.village.VillagerProfession
import net.minecraft.world.World

val bloodLevelPackeId = Identifier("haema:bloodlevelsync")

val goodBloodTag = TagRegistry.entityType(Identifier("haema:good_blood_sources"))
val mediumBloodTag = TagRegistry.entityType(Identifier("haema:medium_blood_sources"))
val poorBloodTag = TagRegistry.entityType(Identifier("haema:poor_blood_sources"))

val vampireEffectiveWeaponsTag = TagRegistry.item(Identifier("haema:vampire_weapons"))

val dungeonLootTable = Identifier("minecraft:chests/simple_dungeon")
val jungleTempleLootTable = Identifier("minecraft:chests/jungle_temple")
val desertPyramidLootTable = Identifier("minecraft:chests/desert_pyramid")

fun init() {
    UseEntityCallback.EVENT.register(UseEntityCallback { player, world, hand, entity, entityHitResult ->
        if ((player as Vampirable).isVampire && entity is LivingEntity && player.isSneaking)
            (player.hungerManager as VampireBloodManager).feed(entity, player)
        else ActionResult.PASS
    })

    UseBlockCallback.EVENT.register(UseBlockCallback { player, world, hand, blockHitResult ->
        val state = world.getBlockState(blockHitResult.blockPos)

        if (state.block !is BedBlock) {
            return@UseBlockCallback ActionResult.PASS
        }

        val pos = if (state.get(BedBlock.PART) == BedPart.HEAD)
            blockHitResult.blockPos
        else
            blockHitResult.blockPos.offset(state.get(BedBlock.FACING))
        val entities = world.getEntities(player, Box(pos)) { it is LivingEntity && it.isSleeping }

        if (entities.isNotEmpty() && (player as Vampirable).isVampire && player.isSneaking) {
            (player.hungerManager as VampireBloodManager).feed(entities[0] as LivingEntity, player)
        } else ActionResult.PASS
    })

    LootTableLoadingCallback.EVENT.register(LootTableLoadingCallback { resourceManager: ResourceManager?, lootManager: LootManager?, id: Identifier?, supplier: FabricLootSupplierBuilder, setter: LootTableSetter? ->
        if (id == dungeonLootTable || id == jungleTempleLootTable || id == desertPyramidLootTable) {
            val poolBuilder: FabricLootPoolBuilder = FabricLootPoolBuilder.builder()
                    .rolls(ConstantLootTableRange.create(1))
                    .withEntry(ItemEntry.builder(Registry.ITEM.get(Identifier("haema:vampire_blood")))
                            .weight(if (id == dungeonLootTable) 10 else 5)
                            .build()
                    )
                    .withEntry(ItemEntry.builder(Items.AIR)
                            .weight(10)
                            .build()
                    )
            supplier.withPool(poolBuilder.build())
        }
    })

    ServerSidePacketRegistry.INSTANCE.register(Identifier("haema:dash")) { packetContext, packetByteBuf ->
        val player = packetContext.player
        val world = player.world
        val target = raytraceForDash(player)

        if (target != null) {
            val rand = world.random
            for (j in 0 until 3) {
                val x: Double = (target.x - player.x) * rand.nextDouble() + player.x - 0.5
                val y: Double = (target.y - player.y) * rand.nextDouble() + player.y + 1
                val z: Double = (target.z - player.z) * rand.nextDouble() + player.z - 0.5
                (world as ServerWorld).spawnParticles(
                        DustParticleEffect.RED,
                        x, y, z,
                        10,
                        0.5, 1.0, 0.5,
                        0.0
                )
            }
            player.teleport(target.x, target.y, target.z)
        }
    }

    Registry.register(
            Registry.STATUS_EFFECT,
            Identifier("haema:sunlight_sickness"),
            SunlightSicknessEffect.instance
    )

    Registry.register(
            Registry.STATUS_EFFECT,
            Identifier("haema:vampiric_strength"),
            VampiricStrengthEffect.instance
    )

    Registry.register(
            Registry.STATUS_EFFECT,
            Identifier("haema:vampiric_weakness"),
            VampiricWeaknessEffect.instance
    )

    Registry.register(
            Registry.ITEM,
            Identifier("haema:vampire_converter"),
            object : Item(Settings()) {
                override fun use(world: World?, user: PlayerEntity?, hand: Hand?): TypedActionResult<ItemStack> {
                    if (world?.isClient != false || user == null)
                        return super.use(world, user, hand)
                    Vampirable.convert(user)
                    return TypedActionResult.consume(user.getStackInHand(hand))
                }
            }
    )

    Registry.register(
            Registry.ITEM,
            Identifier("haema:vampire_blood"),
            object : Item(Item.Settings().group(ItemGroup.MISC)) {
                override fun appendTooltip(stack: ItemStack?, world: World?, tooltip: MutableList<Text>, context: TooltipContext?) {
                    super.appendTooltip(stack, world, tooltip, context)
                    tooltip.add(TranslatableText("item.haema.vampire_blood.desc").formatted(Formatting.DARK_RED))
                }
            }
    )

    Registry.register(
            Registry.ITEM,
            Identifier("haema:vampire_blood_injector"),
            VampireBloodInjectorItem(Item.Settings().group(ItemGroup.TOOLS).maxCount(1))
    )

    Registry.register(
            Registry.ITEM,
            Identifier("haema:empty_vampire_blood_injector"),
            EmptyVampireBloodInjectorItem(Item.Settings().group(ItemGroup.TOOLS).maxCount(1))
    )

    Registry.register(
        Registry.RECIPE_SERIALIZER,
        Identifier("haema:book_of_blood"),
        BookOfBloodRecipe.Serializer
    )

    addTradesToProfession(
            VillagerProfession.CLERIC,
            3,
            TradeOffers.Factory { _, _ ->
                TradeOffer(
                        ItemStack(Items.EMERALD, 5),
                        ItemStack(Registry.ITEM.get(Identifier("haema:vampire_blood"))),
                        1,
                        30,
                        0.05f
                )
            }
    )

    DispenserBlock.registerBehavior(Registry.ITEM.get(Identifier("haema:vampire_blood_injector")), object : FallibleItemDispenserBehavior() {
        override fun dispenseSilently(pointer: BlockPointer, stack: ItemStack): ItemStack {
            val blockPos = pointer.blockPos.offset(pointer.blockState.get(DispenserBlock.FACING))
            val user = pointer.world.getEntities(PlayerEntity::class.java, Box(blockPos), null)
                    .firstOrNull() ?: return stack

            val emptyStack = ItemStack(Registry.ITEM.get(Identifier("haema:empty_vampire_blood_injector")))
            if ((user as Vampirable).isVampire) {
                (user.hungerManager as VampireBloodManager).addBlood(6.0)
                return emptyStack
            }

            if (!user.hasStatusEffect(StatusEffects.STRENGTH) || user.getStatusEffect(StatusEffects.STRENGTH)!!.amplifier <= 0) {
                user.addStatusEffect(StatusEffectInstance(StatusEffects.WITHER, 3000))
                user.addStatusEffect(StatusEffectInstance(StatusEffects.NAUSEA, 100))
                user.damage(IncompatibleBloodDamageSource.instance, 20f)
                return emptyStack
            }

            Vampirable.convert(user)
            return emptyStack
        }
    })

    DispenserBlock.registerBehavior(Registry.ITEM.get(Identifier("haema:empty_vampire_blood_injector")), object : FallibleItemDispenserBehavior() {
        override fun dispenseSilently(pointer: BlockPointer, stack: ItemStack): ItemStack {
            val blockPos = pointer.blockPos.offset(pointer.blockState.get(DispenserBlock.FACING))
            val user = pointer.world.getEntities(PlayerEntity::class.java, Box(blockPos), null)
                    .firstOrNull() ?: return stack

            if ((user as Vampirable).isVampire) {
                if (user.hasStatusEffect(StatusEffects.WEAKNESS) && !(user as Vampirable).isPermanentVampire) {
                    (user as Vampirable).isVampire = false
                    user.kill()
                    return ItemStack(Registry.ITEM.get(Identifier("haema:vampire_blood_injector")));
                }
                if ((user.hungerManager as VampireBloodManager).absoluteBloodLevel < 6.0)
                    return stack
                (user.hungerManager as VampireBloodManager).removeBlood(6.0)
                return ItemStack(Registry.ITEM.get(Identifier("haema:vampire_blood_injector")))
            }

            return stack
        }
    })
}

