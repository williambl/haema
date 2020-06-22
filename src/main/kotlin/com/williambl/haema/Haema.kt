package com.williambl.haema

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
import net.minecraft.client.world.ClientWorld
import net.minecraft.entity.LivingEntity
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
import net.minecraft.util.ActionResult
import net.minecraft.util.Hand
import net.minecraft.util.Identifier
import net.minecraft.util.TypedActionResult
import net.minecraft.util.hit.BlockHitResult
import net.minecraft.util.hit.HitResult
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.Direction
import net.minecraft.util.math.Vec3d
import net.minecraft.util.registry.Registry
import net.minecraft.village.TradeOffer
import net.minecraft.village.TradeOffers
import net.minecraft.village.VillagerProfession
import net.minecraft.world.RayTraceContext
import net.minecraft.world.World

val bloodLevelPackeId = Identifier("haema:bloodlevelsync")

val goodBloodTag = TagRegistry.entityType(Identifier("haema:good_blood_sources"))
val mediumBloodTag = TagRegistry.entityType(Identifier("haema:medium_blood_sources"))
val poorBloodTag = TagRegistry.entityType(Identifier("haema:poor_blood_sources"))

val vampireEffectiveWeaponsTag = TagRegistry.item(Identifier("haema:vampire_weapons"))

val dungeonLootTable = Identifier("minecraft:chests/simple_dungeon")
val jungleTempleLootTable = Identifier("minecraft:chests/jungle_temple")
val desertPyramidLootTable = Identifier("minecraft:chests/desert_pyramid")

//TODO: patchouli
fun init() {
    UseEntityCallback.EVENT.register(UseEntityCallback { player, world, hand, entity, entityHitResult ->
        if ((player as Vampirable).isVampire && entity is LivingEntity && player.isSneaking)
            (player.hungerManager as VampireBloodManager).feed(entity, player)
        else ActionResult.PASS
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
            Item(Item.Settings().group(ItemGroup.MISC))
    )

    Registry.register(
            Registry.ITEM,
            Identifier("haema:vampire_blood_injector"),
            VampireBloodInjectorItem(Item.Settings().group(ItemGroup.TOOLS))
    )

    Registry.register(
            Registry.ITEM,
            Identifier("haema:empty_vampire_blood_injector"),
            EmptyVampireBloodInjectorItem(Item.Settings().group(ItemGroup.TOOLS))
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
}

